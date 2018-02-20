{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Turnip.Eval.Eval where

import qualified Turnip.AST as AST
import Turnip.Eval.Types
import Turnip.Eval.Util
import Turnip.Eval.Closure

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Debug.Trace

padWithNils :: Int -> [Value] -> [Value]
padWithNils n xs = xs ++ replicate (n - length xs) Nil

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

    varargsData <- if hasVarargs
        then do
            -- varargs are 'leftover' arguments, essentially
            let vargs = drop (length names) args
            tr <- makeNewTableWith . Map.fromList $ (zip (map Number [1..]) vargs)
            -- return the binding to those values
            -- note: this table can be empty, but ti
            return [(Str "arg", Table tr)]
        else
            return []

    -- this is table data containing arguments
    let argsTableData = Map.fromList $ (argsWithNames ++ varargsData)

    -- we turn it into a regular, registered table
    newCls <- makeNewTableWith argsTableData
    -- and append it to the closure stack
    -- together with the closure stored in the functiondata
    foldl (flip closurePush) b ((ClosureLevel newCls (Just args)):cls)
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
        Nothing -> throwError "Ellipsis eval'd outside of a varargs function"

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
        x -> throwError $ "Trying to call something that doesn't eval to a function! (" ++ show x ++ ")"

eval (AST.MemberCall obj fName args) = do
    argVs <- map head <$> mapM eval args
    objV <- head <$> eval obj
    case objV of
        Table tr -> do
            fV <- getTableField tr (Str fName)
            case fV of
                -- objV is prepended to the argument list as the 'self' parameter
                Function ref -> callRef ref (objV : argVs)
                x -> throwError $ "Attempt to call method '" ++ fName ++ "' (" ++ show x ++ ")"
        _ -> throwError $ "Attempt to index a non-table (" ++ show objV ++ ")"

eval (AST.FieldRef t k) = do
    tv <- head <$> eval t

    -- we ignore any values returned by the expression because
    -- we only want to index the first one anyway
    case tv of
        (Table tRef) -> do
            -- similarly here
            kV <- head <$> eval k

            t <- getTableData tRef
            let mVal :: Maybe Value = t ^. at kV
            return $ [extractVal mVal]

        _ -> throwError $ "Attempt to index a non-table (" ++ show tv ++ ")"

-- this is essentially the same as regular call
-- TODO should it even be a difference in the AST?
eval (AST.BinOp name lhs rhs) = eval (AST.Call (AST.Var name) [lhs, rhs])
eval (AST.UnOp name expr) = eval (AST.Call (AST.Var name) [expr])

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

execStmt (AST.If blocks mElseB) = do
    -- if an else block is present, we can append it to the list
    -- with a predicate that always evals to True.
    let blocks' = case mElseB of
                    Just elseB -> blocks ++ [(AST.Bool True, elseB)]
                    Nothing -> blocks

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
            _ -> throwError "'for' limits and step must be numbers"

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
        _ -> throwError "The iterator is not a function" 

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
        Table tr -> setTableField tr (kv,v)
        _ -> throwError "Trying to assign to a field of non-table"