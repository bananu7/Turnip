{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Turnip.Eval.Eval where

import qualified Turnip.AST as AST
import Turnip.Eval.Types
import Turnip.Eval.Util
import Turnip.Eval.Metatables
import Turnip.Eval.Closure

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Fixed (mod')

padWithNils :: Int -> [Value] -> [Value]
padWithNils n xs = xs ++ replicate (n - length xs) Nil

callMeta :: TableRef -> [Value] -> LuaM [Value]
callMeta tr args = do
    let self = Table tr
    maybeFn <- getMetaFunction "__call" self

    case maybeFn of
        Just fr -> callRef fr (self:args)
        _ -> throwErrorStr "Attempt to call a table without a __call metafunction"

callRef :: FunctionRef -> [Value] -> LuaM [Value]
callRef f args = do
    fd <- getFunctionData f
    call fd args

call :: FunctionData -> [Value] -> LuaM [Value]
call (BuiltinFunction fn) args = do
    -- ensure args match signature
    -- if yes, extract them into list of Haskell values ? what about Haskell functions operating on Lua-level values ?
    result <- fn args
    -- call the fn and take its result
    -- possibly with liftIO
    return result

call (FunctionData cls block names hasVarargs) args = do
    -- for every arg set cls[names[i]] = args[i]
    -- TODO: in case of a (trailing) vararg function, set `arg` variable to hold
    -- (the REST of) the arguments

    -- If not enough parameters were passed, the missing ones must still appear as nils
    -- otherwise they wouldn't appear in the closure
    let argsWithNils = padWithNils (length names - length args) args
    let argsWithNames = zip (map Str names) argsWithNils

    newClosureLevel <- do
        -- varargs are 'leftover' arguments, essentially
        let varargs = drop (length names) args

        varargsData <- if hasVarargs
            then do
                -- 'arg' value
                tr <- makeNewTableWith . Map.fromList $ (zip (map Number [1..]) varargs)
                return [(Str "arg", Table tr)]
            else
                return []

        let ellipsisData = if hasVarargs
            then Just varargs
            else Nothing

        let argsTableData = Map.fromList $ (argsWithNames ++ varargsData)
        -- we turn arguments into a regular, registered tableś
        newCls <- makeNewTableWith argsTableData

        return $ ClosureLevel newCls ellipsisData

    -- and append it to the closure stack
    -- together with the closure stored in the functiondata
    foldl (flip closurePush) b (newClosureLevel:cls)
     where
        b = do
            res <- execBlock block
            case res of
                ReturnBubble vs -> return vs
                _ -> return [Nil]


eval :: AST.Expr -> LuaM [Value]
-- Literals don't use the closure parameter
eval (AST.Number n) = return [Number n]
eval (AST.StringLiteral _ str) = return [Str str]
eval (AST.Bool b) = return [Boolean b]
eval AST.Nil = return [Nil]

-- In order to eval ellipsis, the closure needs to differentiate between
-- outer local variables and parameters. It's basically the same as args, but
-- not packed in a table.
eval AST.Ellipsis = do
    e <- closureLookupEllipsis
    case e of
        Just v -> return v
        Nothing -> throwErrorStr "Ellipsis eval'd outside of a varargs function"

-- lambda needs to be stored in the function table
eval (AST.Lambda parNames varargs b) = do
    cls <- getClosure
    newRef <- makeNewLambda $ FunctionData cls b parNames varargs
    return [Function newRef]

eval (AST.Var name) = (:[]) <$> closureLookup (Str name)

eval (AST.Call fn args) = do
    -- theoretically always a Nil should be returned, but
    -- it's not in the type system yet. (wrt head)
    argVs <- map head <$> mapM (\a -> eval a) args
    fnV <- head <$> eval fn

    case fnV of 
        Function ref -> callRef ref argVs
        Table tref -> callMeta tref argVs
        x -> throwErrorStr $ "Trying to call something that doesn't eval to a function! (" ++ show x ++ ")"

eval (AST.MemberCall obj fName args) = do
    argVs <- map head <$> mapM eval args
    objV <- head <$> eval obj
    case objV of
        Table tr -> do
            fV <- getTableField tr (Str fName)
            case fV of
                -- objV is prepended to the argument list as the 'self' parameter
                Function ref -> callRef ref (objV : argVs)
                x -> throwErrorStr $ "Attempt to call method '" ++ fName ++ "' (" ++ show x ++ ")"
        _ -> throwErrorStr $ "Attempt to index a non-table (" ++ show objV ++ ")"

eval (AST.FieldRef t k) = do
    -- we ignore any values returned by the expression because
    -- we only want to index the first one anyway
    tv <- head <$> eval t
    -- similarly the composite index keys just don't work and the first value is used
    kv <- head <$> eval k

    case tv of
        Table tRef -> getTableFieldWithMetatable tRef kv
        _ -> throwErrorStr $ "Attempt to index a non-table (" ++ show tv ++ ")"

    where
        getTableFieldWithMetatable :: TableRef -> Value -> LuaM [Value]
        getTableFieldWithMetatable tr tk = 
            rawGetTableField tr tk >>= \mv -> case mv of
                Just v -> return [v]
                Nothing -> do
                    maybeMetaTr <- getMetatable (Table tr)
                    case maybeMetaTr of
                        Just metaTr -> do
                            maybeMetaIndex <- rawGetTableField metaTr (Str "__index")
                            case maybeMetaIndex of
                                Just (Table metaTabRef) -> getTableFieldWithMetatable metaTabRef tk
                                Just (Function metaFunRef) -> callRef metaFunRef [(Table metaTr), tk]
                                _ -> return [Nil]
                        Nothing -> return [Nil]

eval (AST.BinOp op lhs rhs) = do
    a <- head <$> eval lhs
    binaryOperatorCall op a rhs

eval (AST.UnOp op expr) = do
    a <- head <$> eval expr
    unaryOperatorCall op a

-- Table constructor in form { k = v, ... }
eval (AST.TableCons entries) = do
    tr <- makeNewTable

    flip evalStateT (1 :: Int) $
        forM_ entries (addEntry tr)

    return [Table tr]
    where
        --addEntry :: TableRef -> (Maybe AST.Expr, AST.Expr) -> StateT Int _ _
        -- The map-like entry
        -- I need to 'lift' here to separate the LuaM rankntype from the StateT
        addEntry tr (Just ek, ev) = lift $ do
            k <- head <$> eval ek
            v <- head <$> eval ev
            setTableField tr (k,v)

        -- The numeric, array-like entry
        addEntry tr (Nothing, ev) = do
            ix <- get
            put $ ix + 1

            lift $ do
                v <- head <$> eval ev
                setTableField tr (Number (fromIntegral ix), v)            

-- TODO - should a comma-separated expression list have a dedicated AST node
evalExpressionList :: [AST.Expr] -> LuaM [Value]
evalExpressionList xs = do
    firsts <- mapM eval (init xs)
    let singular = map head firsts
    pack <- eval (last xs)

    return $ singular ++ pack

--------------

-- The second param to Binary Operators is an expression to allow
-- short-circuiting of and/or operators.
type BinaryOperatorImpl = Value -> Value -> LuaM [Value]
type UnaryOperatorImpl = Value -> LuaM [Value]

binaryOperatorCall :: AST.BinaryOperator -> Value -> AST.Expr -> LuaM [Value]
binaryOperatorCall AST.OpRaise = strictBinaryOp opRaise
binaryOperatorCall AST.OpPlus = strictBinaryOp opPlus
binaryOperatorCall AST.OpMinus = strictBinaryOp opMinus
binaryOperatorCall AST.OpMult = strictBinaryOp opMult
binaryOperatorCall AST.OpDivide = strictBinaryOp opDiv
binaryOperatorCall AST.OpModulo = strictBinaryOp opModulo
binaryOperatorCall AST.OpConcat = strictBinaryOp opConcat
binaryOperatorCall AST.OpEqual = strictBinaryOp opEqual
binaryOperatorCall AST.OpLess = strictBinaryOp opLess
binaryOperatorCall AST.OpGreater = strictBinaryOp opGreater
binaryOperatorCall AST.OpLE = strictBinaryOp opLE
binaryOperatorCall AST.OpGE = strictBinaryOp opGE
binaryOperatorCall AST.OpNotEqual = strictBinaryOp opNotEqual
binaryOperatorCall AST.OpAnd = opAnd
binaryOperatorCall AST.OpOr = opOr

strictBinaryOp :: BinaryOperatorImpl -> Value -> AST.Expr -> LuaM [Value]
strictBinaryOp op a rhs  = do
    b <- head <$> eval rhs
    op a b

unaryOperatorCall :: AST.UnaryOperator -> Value -> LuaM [Value]
unaryOperatorCall AST.OpUnaryMinus = opUnaryMinus
unaryOperatorCall AST.OpLength = opLength
unaryOperatorCall AST.OpNot = opNot

{-
  https://www.lua.org/pil/13.1.html
  To choose a metamethod, Lua does the following:
    (1) If the first value has a metatable with an __add field, Lua uses this value as the metamethod,
        independently of the second value;
    (2) otherwise, if the second value has a metatable with an __add field, Lua uses this value as the metamethod;
    (3) otherwise, Lua raises an error.

    __add, __mul, __sub (for subtraction), __div (for division),
    __unm (for negation), and __pow
-}

binaryMetaOperator :: String -> Value -> Value -> LuaM [Value]
binaryMetaOperator fstr a b = do
    maybeFn <- getMetaFunction fstr a
    case maybeFn of
        Just fra -> callRef fra [a,b]
        _ -> do
            maybeFnB <- getMetaFunction fstr b
            case maybeFnB of
                Just frb -> callRef frb [a,b]
                _ -> throwErrorStr $ "No metaop '" ++ fstr ++ "' on those two values"

unaryMetaOperator :: String -> Value -> LuaM [Value]
unaryMetaOperator fstr a = do
    maybeFn <- getMetaFunction fstr a
    case maybeFn of
        Just fr -> callRef fr [a]
        _ -> throwErrorStr $ "No metaop '" ++ fstr ++ "' on this value"


opPlus :: BinaryOperatorImpl
opPlus (Number a) (Number b) = return $ [Number (a + b)]
opPlus a b = binaryMetaOperator "__add" a b

opMult :: BinaryOperatorImpl
opMult (Number a) (Number b) = return $ [Number (a * b)]
opMult a b = binaryMetaOperator "__mult" a b

opDiv :: BinaryOperatorImpl
opDiv (Number a) (Number b) = return $ [Number (a / b)]
opDiv a b = binaryMetaOperator "__div" a b

opUnaryMinus :: UnaryOperatorImpl
opUnaryMinus (Number a) = return $ [Number (-a)] --unary negate
opUnaryMinus a = unaryMetaOperator "__unm" a

opMinus :: BinaryOperatorImpl
opMinus (Number a) (Number b) = return $ [Number (a - b)]
opMinus a b = binaryMetaOperator "__sub" a b

opRaise :: BinaryOperatorImpl
opRaise (Number a) (Number b) = return [Number $ a ** b]
opRaise a b = binaryMetaOperator "__pow" a b

opModulo :: BinaryOperatorImpl
opModulo (Number a) (Number b) = return [Number $ a `mod'` b]
opModulo a b = binaryMetaOperator "__mod" a b

opConcat :: BinaryOperatorImpl
opConcat (Str a) (Str b) = return [Str $ a ++ b]
opConcat a b = binaryMetaOperator "__concat" a b

opLength :: UnaryOperatorImpl
opLength (Str a) = return [Number . fromIntegral $ length a]
opLength (Table tr) = do
    hasMetaLen <- isJust <$> getMetaFunction "__len" (Table tr)
    if hasMetaLen
        then unaryMetaOperator "__len" (Table tr)
        else do
            (TableData td _) <- getTableData tr
            case Map.lookupMax td of
                Just (Number x, _) -> return [Number x]
                _ -> return [Number 0]

opLength Nil = throwErrorStr "Attempt to get length of a nil value" -- :)
opLength a = unaryMetaOperator "__len" a

-- Polymorphic comparison operators
opEqual :: BinaryOperatorImpl
opEqual Nil Nil = return [Boolean True]
opEqual a b
    | a == b = return [Boolean True]
    | otherwise = (:[]) . Boolean <$> eqHelper a b

opNotEqual :: BinaryOperatorImpl
opNotEqual Nil Nil = return [Boolean False]
opNotEqual a b 
    | a == b = return [Boolean False]
    | otherwise = (:[]) . Boolean . not <$> eqHelper a b

eqHelper :: Value -> Value -> LuaM Bool
eqHelper a b = do
    maybeEqA <- getMetaFunction "__eq" a
    maybeEqB <- getMetaFunction "__eq" b

    case (maybeEqA, maybeEqB) of
        -- meta-equality is only used if both eq functions are the same
        (Just eqA, Just eqB) | eqA == eqB -> coerceToBool <$> callRef eqA [a,b]
        _ -> return False

opLess :: BinaryOperatorImpl
opLess (Number a) (Number b) = return [Boolean $ a < b]
opLess (Str a) (Str b) = return [Boolean $ a < b]
opLess a b = binaryMetaOperator "__lt" a b

opGreater :: BinaryOperatorImpl
opGreater a b = opLess b a

opLE :: BinaryOperatorImpl
opLE (Number a) (Number b) = return [Boolean $ a <= b]
opLE (Str a) (Str b) = return [Boolean $ a <= b]
opLE a b = binaryMetaOperator "__le" a b

opGE :: BinaryOperatorImpl
opGE a b = opLE b a

opNot :: UnaryOperatorImpl
opNot a = return [Boolean . not . coerceToBool $ [a]]

opOr :: Value -> AST.Expr -> LuaM [Value]
opOr a rhs = do
    if coerceToBool [a] then
        return [a]
    else do
        b <- head <$> eval rhs
        return [b]

opAnd :: Value -> AST.Expr -> LuaM [Value]
opAnd a rhs = do
    if not $ coerceToBool [a] then
        return [a]
    else do
        b <- head <$> eval rhs
        return [b]

--------------

runUntil :: Monad m => [a] -> (a -> m Bubble) -> m Bubble
runUntil (h:t) f = do
    r <- f h
    case r of
        -- if there was no bubble breaking the block execution
        -- just move on to the next statement
        EmptyBubble -> runUntil t f
        x -> return x
        
runUntil [] _ = return EmptyBubble

execBlock :: AST.Block -> LuaM Bubble
execBlock (AST.Block stmts) = runUntil stmts $ \stmt -> execStmt stmt

execStmt :: AST.Stmt -> LuaM Bubble

execStmt (AST.If block ifElseBlocks mElseB) = do
    -- if an else block is present, we can append it to the list
    -- with a predicate that always evals to True.
    let blocks' = block : ifElseBlocks ++ elseBlock
                    where
                        elseBlock = case mElseB of
                            Just elseB -> [(AST.Bool True, elseB)]
                            Nothing -> []

    runUntil blocks' $ \(expr, b) -> do
        result <- coerceToBool <$> eval expr
        if result
          then execBlock b
          else return EmptyBubble

execStmt (AST.For names (AST.ForNum emin emax mestep) b) = do
    step <- case mestep of
        Just estep -> head <$> eval estep
        Nothing -> pure $ Number 1.0

    vmin <- head <$> eval emin
    vmax <- head <$> eval emax

    newCls <- makeNewTableWith . Map.fromList $ map (\n -> (Str n, Nil)) names

    closurePush (ClosureLevel newCls Nothing) $ do
        case (vmin, vmax, step) of
            (Number i, Number n, Number s) -> loopBody i n s
            _ -> throwErrorStr "'for' limits and step must be numbers"

        where
            loopBody i n step = do
                let cont = if step > 0 then i <= n else i >= n
                if cont then do
                    -- TODO: duplication between numeric and generic for
                    execAssignment (map AST.LVar names) [Number i]
                    blockResult <- execBlock b
                    let i' = i + step
                    case blockResult of
                        EmptyBubble -> loopBody i' n step
                        BreakBubble -> return EmptyBubble
                        x -> return x
                else
                    return EmptyBubble

-- The semantics for that version have been taken from PIL 7.2
-- https://www.lua.org/pil/7.2.html
execStmt (AST.For names (AST.ForIter explist) b) = do
    -- Like in a multiple assignment, only the last (or the only)
    -- element of the list can result in more than one value;
    -- and the number of values is adjusted to three, extra
    -- values being discarded or nils added as needed.
    -- (When we use simple iterators, the factory returns
    -- only the iterator function, so the invariant state
    -- and the control variable get nil.)
    [f, s, var] <- padWithNils 3 <$> evalExpressionList explist

    -- A function reference is (hopefully )returned after evaluating
    -- the explist
    fv <- case f of
        Function fref -> getFunctionData fref 
        _ -> throwErrorStr "The iterator is not a function" 

    newCls <- makeNewTableWith . Map.fromList $ map (\n -> (Str n, Nil)) names

    closurePush (ClosureLevel newCls Nothing) $ do

        loopBody fv s var
        where
            loopBody fv s var = do
                -- the first value is the "iterator"
                vars <- call fv [s, var]
                -- the rest are put in the local variables
                execAssignment (map AST.LVar names) vars

                let var' = head vars
                if coerceToBool [var']
                    then do
                        -- TODO: duplication between numeric and generic for
                        blockResult <- execBlock b
                        case blockResult of
                            EmptyBubble -> loopBody fv s var'
                            BreakBubble -> return EmptyBubble
                            x -> return x
                    else
                        return EmptyBubble

execStmt (AST.While e b) = do
    result <- coerceToBool <$> eval e
    if result then do
        blockResult <- execBlock b
        case blockResult of
            -- In case the inner block didn't break, just recurse
            EmptyBubble -> execStmt (AST.While e b)
            -- While 'contains' the break bubble and turns it into
            -- an empty one, closing the statement.
            BreakBubble -> return EmptyBubble
            -- Any other bubble (like return) can't be handled, 
            -- and is forwarded.
            x -> return x
    else
        return EmptyBubble
    -- if no change has been made to lua state, it can be safely assumed that it's
    -- an infinite loop

-- The only thing "do..end" does is introduce an empty closure level
-- where local vals are stored
execStmt (AST.Do b) = do
    newCls <- makeNewTableWith Map.empty
    closurePush (ClosureLevel newCls Nothing) $ execBlock b

execStmt (AST.Until e b) = do
    blockResult <- execBlock b
    case blockResult of
        EmptyBubble -> do
            result <- coerceToBool <$> eval e
            if not result then execStmt (AST.Until e b)
                          else return EmptyBubble
        BreakBubble -> return EmptyBubble
        x -> return x

execStmt AST.Break = return BreakBubble

-- call statement is a naked call expression with result ignored
execStmt (AST.CallStmt f ps) = do
    _ <- eval (AST.Call f ps)
    return EmptyBubble

execStmt (AST.MemberCallStmt obj f ps) = do
    _ <- eval (AST.MemberCall obj f ps)
    return EmptyBubble

-- this is a special case of an unpacking assignment
execStmt (AST.Assignment lvals [expr]) = do
    vals <- eval expr
    execAssignment lvals vals
    return EmptyBubble

-- this is "regular" multiple assignment
execStmt (AST.Assignment lvals exprs) = do
    -- this takes the first value of every expression
    -- it only happens when there are more than 1 expr on rhs
    vals <- mapM (\e -> head <$> eval e) exprs
    execAssignment lvals vals
    return EmptyBubble

-- LocalDef is very similar to regular assignment
execStmt (AST.LocalDecl names) = do
    cls <- getClosure
    declTarget :: TableRef <- case cls of
        (topCls:_) -> pure . closureTableRef $ topCls
        _ -> getGlobalTableRef

    -- we have to force using this target here to create new names
    -- in the top level closure; assignmentTarget only uses existing ones
    mapM_ (\name -> setTableField declTarget (Str name, Nil)) names

    return EmptyBubble

execStmt (AST.Return exprs) = do
    -- if there's only one expression to return, and it evals into
    -- multiple values, it needs to be forwarded
    vals <- case exprs of
        [singleExpr] -> eval singleExpr
        multipleExprs -> map head <$> mapM (\e -> eval e) multipleExprs

    return $ ReturnBubble vals

execAssignment :: [AST.LValue] -> [Value] -> LuaM ()
execAssignment lvals vals = do
    -- fill in the missing Nil-s for zip
    -- let valsPadded = padWithNils (length lvals) vals

    -- because of how zipWith works, this isn't necessary; namely, only
    -- the assignments that have the vals are executed at all
    -- this does not impact local definitions, because they are executed with
    -- separate declarator statements NOW. Since that could change in the future,
    -- I'm keeping the above code for reference, if a need to pad the assingment
    -- with Nils appears.

    sequence_ $ zipWith assignLValue lvals vals

assignLValue :: AST.LValue -> Value -> LuaM ()
assignLValue (AST.LVar name) v = do
    target <- assignmentTarget name
    setTableField target (Str name, v)

assignLValue (AST.LFieldRef t k) v = do
    tv <- head <$> eval t
    kv <- head <$> eval k
    case tv of
        Table tr -> setTableFieldWithNewindex tr (kv,v)
        _ -> throwErrorStr "Trying to assign to a field of non-table"
    where
        setTableFieldWithNewindex :: TableRef -> (Value,Value) -> LuaM ()
        setTableFieldWithNewindex tr (tk, tv) = 
            rawGetTableField tr tk >>= \mv -> case mv of
                -- if key is already present, do regular insert
                Just _ -> regularSet
                -- if not, try the metatable
                Nothing -> do
                    maybeMetaTr <- getMetatable (Table tr)
                    case maybeMetaTr of
                        Just metaTr -> do
                            -- see if it has metaindex
                            maybeNewIndex <- rawGetTableField metaTr (Str "__newindex")
                            case maybeNewIndex of
                                Just (Function metaFunRef) -> callRef metaFunRef [(Table tr), tk, tv] >> return ()
                                _ -> regularSet
                        Nothing -> regularSet
            where
                regularSet = setTableField tr (tk, tv)


