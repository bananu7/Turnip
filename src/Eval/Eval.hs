{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Eval.Eval where

import qualified Data.Map as Map
import qualified LuaAS as AST
import Eval.Types
import Eval.Util

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Debug.Trace

-- This function fills in the Nil for the missing Value
extractVal :: Maybe Value -> Value
extractVal Nothing = Nil
extractVal (Just v) = v

padWithNils :: Int -> [Value] -> [Value]
padWithNils n xs = xs ++ replicate (n - length xs) Nil

closureLookup :: Value -> Closure -> LuaM Value
-- descend recursively with lookups, picking the closest name first
closureLookup v (topRef:cls) = do
    topCls <- getTableData topRef
    case Map.lookup v topCls of
        Just val -> return val
        Nothing -> closureLookup v cls
-- if closure lookup fails, try global lookup
closureLookup v _ = do  
    _G <- getGlobalTable
    let mVal = Map.lookup v _G
    return $ extractVal mVal

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

call (FunctionData cls block names) args = do
    -- for every arg set cls[names[i]] = args[i]
    -- TODO: in case of a (trailing) vararg function, set `arg` variable to hold
    -- (the REST of) the arguments

    -- this is table data containing arguments
    let argsTableData = Map.fromList $ zip (map Str names) args
    -- we turn it into a regular, registered table
    newCls <- makeNewTableWith argsTableData
    -- and append it to the closure stack
    let clsWithArgs = newCls : cls

    res <- execBlock block clsWithArgs
    case res of
        ReturnBubble vs -> return vs
        _ -> return [Nil]

eval :: AST.Expr -> Closure -> LuaM [Value]
-- Literals don't use the closure parameter
eval (AST.Number n) _ = return [Number n]
eval (AST.StringLiteral _ str) _ = return [Str str]
eval (AST.Bool b) _ = return [Boolean b]
eval AST.Nil _ = return [Nil]

-- In order to eval ellipsis, the closure needs to differentiate between
-- outer local variables and parameters
eval AST.Ellipsis _ = throwError "how do you even eval ellipsis"

-- lambda needs to be stored in the function table
eval (AST.Lambda parNames b) cls = do
    newRef <- uniqueFunctionRef
    functions . at newRef .= (Just $ FunctionData cls b parNames)
    return [Function newRef]

eval (AST.Var name) cls = (:[]) <$> closureLookup (Str name) cls

eval (AST.Call fn args) cls = do
    -- theoretically always a Nil should be returned, but
    -- it's not in the type system yet. (wrt head)
    argVs <- map head <$> mapM (\a -> eval a cls) args
    fnV <- eval fn cls

    case fnV of 
        (Function ref:_) -> callRef ref argVs
        x -> throwError $ "Trying to call something that doesn't eval to a function! (" ++ show x ++ ")"

eval (AST.MemberCall obj fName args) cls = do
    argVs <- map head <$> mapM (\a -> eval a cls) args
    objV <- head <$> eval obj cls
    case objV of
        Table tr -> do
            fV <- getTableField tr (Str fName)
            case fV of
                Function ref -> callRef ref argVs
                x -> throwError $ "Attempt to call method '" ++ fName ++ "' (" ++ show x ++ ")"
        _ -> throwError $ "Attempt to index a non-table (" ++ show objV ++ ")"

eval (AST.FieldRef t k) cls = do
    tv <- head <$> eval t cls

    -- we ignore any values returned by the expression because
    -- we only want to index the first one anyway
    case tv of
        (Table tRef) -> do
            -- similarly here
            kV <- head <$> eval k cls

            t <- getTableData tRef
            let mVal :: Maybe Value = t ^. at kV
            return $ [extractVal mVal]

        _ -> throwError $ "Attempt to index a non-table (" ++ show tv ++ ")"

-- this is essentially the same as regular call
-- TODO should it even be a difference in the AST?
eval (AST.BinOp name lhs rhs) cls = eval (AST.Call (AST.Var name) [lhs, rhs]) cls
eval (AST.UnOp name expr) cls = eval (AST.Call (AST.Var name) [expr]) cls

-- Table constructor in form { k = v, ... }
eval (AST.TableCons entries) cls = do
    tr <- makeNewTable

    flip evalStateT 1 $
        forM_ entries (addEntry tr)

    return [Table tr]
    where
        --addEntry :: TableRef -> (Maybe AST.Expr, AST.Expr) -> StateT Int _ _
        -- The map-like entry
        -- I need to 'lift' here to separate the LuaM rankntype from the StateT
        addEntry tr (Just ek, ev) = lift $ do
            k <- head <$> eval ek cls
            v <- head <$> eval ev cls
            setTableField tr (k,v)

        -- The numeric, array-like entry
        addEntry tr (Nothing, ev) = do
            ix <- get
            put $ ix + 1

            lift $ do
                v <- head <$> eval ev cls
                setTableField tr (Number (fromIntegral ix), v)            

-- TODO - should a comma-separated expression list have a dedicated AST node
evalExpressionList :: [AST.Expr] -> Closure -> LuaM [Value]
evalExpressionList xs cls = do
    firsts <- mapM (flip eval cls) (init xs)
    let singular = map head firsts
    pack <- eval (last xs) cls

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

execBlock :: AST.Block -> Closure -> LuaM Bubble
execBlock (AST.Block stmts) cls = runUntil stmts $ \stmt -> do
    case stmt of
        -- the only statement that return values is Return
        AST.Return exprs -> execReturnStatement exprs cls
        _ -> execStmt stmt cls

execStmt :: AST.Stmt -> Closure -> LuaM Bubble

execStmt (AST.If blocks mElseB) cls = do
    -- if an else block is present, we can append it to the list
    -- with a predicate that always evals to True.
    let blocks' = case mElseB of
                    Just elseB -> blocks ++ [(AST.Bool True, elseB)]
                    Nothing -> blocks

    runUntil blocks' $ \(expr, b) -> do
        result <- coerceToBool <$> eval expr cls
        if result
          then execBlock b cls
          else return EmptyBubble

execStmt (AST.For names (AST.ForNum emin emax mestep) b) cls = do
    newCls <- makeNewTableWith . Map.fromList $ map (\n -> (Str n, Nil)) names
    let cls' = newCls : cls

    step <- case mestep of
        Just estep -> head <$> eval estep cls
        Nothing -> pure $ Number 1.0

    vmin <- head <$> eval emin cls
    vmax <- head <$> eval emax cls

    case (vmin, vmax, step) of
        (Number i, Number n, Number s) -> loopBody cls' i n s
        _ -> throwError "'for' limits and step must be numbers"

    where
        loopBody cls i n step = do
            let cont = if step > 0 then i <= n else i >= n
            if cont then do
                -- TODO: duplication between numeric and generic for
                execAssignment cls (map AST.LVar names) [Number i]
                blockResult <- execBlock b cls
                let i' = i + step
                case blockResult of
                    EmptyBubble -> loopBody cls i' n step
                    BreakBubble -> return EmptyBubble
                    x -> return x
            else
                return EmptyBubble

-- The semantics for that version have been taken from PIL 7.2
-- https://www.lua.org/pil/7.2.html
execStmt (AST.For names (AST.ForIter explist) b) cls = do
    -- Like in a multiple assignment, only the last (or the only)
    -- element of the list can result in more than one value;
    -- and the number of values is adjusted to three, extra
    -- values being discarded or nils added as needed.
    -- (When we use simple iterators, the factory returns
    -- only the iterator function, so the invariant state
    -- and the control variable get nil.)
    [f, s, var] <- padWithNils 3 <$> evalExpressionList explist cls

    -- A function reference is (hopefully )returned after evaluating
    -- the explist
    fv <- case f of
        Function fref -> getFunctionData fref 
        _ -> throwError "The iterator is not a function" 

    newCls <- makeNewTableWith . Map.fromList $ map (\n -> (Str n, Nil)) names
    let cls' = newCls : cls

    loopBody cls' fv s var
    where
        loopBody cls fv s var = do
            -- the first value is the "iterator"
            vars <- call fv [s, var]
            -- the rest are put in the local variables
            execAssignment cls (map AST.LVar names) vars

            let var' = head vars
            if coerceToBool [var']
                then do
                    -- TODO: duplication between numeric and generic for
                    blockResult <- execBlock b cls
                    case blockResult of
                        EmptyBubble -> loopBody cls fv s var'
                        BreakBubble -> return EmptyBubble
                        x -> return x
                else
                    return EmptyBubble

execStmt (AST.While e b) cls = do
    result <- coerceToBool <$> eval e cls
    if result then do
        blockResult <- execBlock b cls
        case blockResult of
            -- In case the inner block didn't break, just recurse
            EmptyBubble -> execStmt (AST.While e b) cls
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

execStmt AST.Break _ = return BreakBubble

-- call statement is a naked call expression with result ignored
execStmt (AST.CallStmt f ps) cls = do
    _ <- eval (AST.Call f ps) cls
    return EmptyBubble

execStmt (AST.MemberCallStmt obj f ps) cls = do
    _ <- eval (AST.MemberCall obj f ps) cls
    return EmptyBubble

-- this is a special case of an unpacking assignment
execStmt (AST.Assignment lvals [expr]) cls = do
    vals <- eval expr cls
    execAssignment cls lvals vals
    return EmptyBubble

-- this is "regular" multiple assignment
execStmt (AST.Assignment lvals exprs) cls = do
    -- this takes the first value of every expression
    -- it only happens when there are more than 1 expr on rhs
    vals <- mapM (\e -> head <$> eval e cls) exprs
    execAssignment cls lvals vals
    return EmptyBubble

-- LocalDef is very similar to regular assignment
execStmt (AST.LocalDecl names) cls = do
    declTarget :: TableRef <- case cls of
        (topCls:_) -> pure topCls
        _ -> use gRef

    -- we have to force using this target here to create new names
    -- in the top level closure; assignmentTarget only uses existing ones
    mapM_ (\name -> setTableField declTarget (Str name, Nil)) names

    return EmptyBubble

-- this is a simple helper that picks either top level closure or global table
assignmentTarget :: Closure -> AST.LValue -> LuaM TableRef
assignmentTarget [] _ = use gRef
-- before choosing local closure for assignment, we should first check
-- whether the value doesn't exist in the closure
-- this is essentially the core of lexical scoping, I suppose
assignmentTarget (topCls:cls) (AST.LVar name) = do
    t <- getTableData topCls
    case Map.lookup (Str name) t of
        -- if the name appears in the closure, we assign to this one
        (Just _) -> return topCls
        -- otherwise we try going down the stack
        Nothing -> assignmentTarget cls (AST.LVar name)

execAssignment :: Closure -> [AST.LValue] -> [Value] -> LuaM ()
execAssignment cls lvals vals = do
    -- fill in the missing Nil-s for zip
    -- let valsPadded = padWithNils (length lvals) vals

    -- because of how zipWith works, this isn't necessary; namely, only
    -- the assignments that have the vals are executed at all
    -- this does not impact local definitions, because they are executed with
    -- separate declarator statements NOW. Since that could change in the future,
    -- I'm keeping the above code for reference, if a need to pad the assingment
    -- with Nils appears.

    sequence_ $ zipWith (assignLValue cls) lvals vals

assignLValue :: Closure -> AST.LValue -> Value -> LuaM ()
assignLValue cls (AST.LVar name) v = do
    target <- assignmentTarget cls (AST.LVar name)
    setTableField target (Str name, v)

assignLValue cls (AST.LFieldRef t k) v = do
    tv <- head <$> eval t cls
    kv <- head <$> eval k cls
    case tv of
        Table tr -> setTableField tr (kv,v)
        _ -> throwError "Trying to assign to a field of non-table"

{-
executionStmt (AST.Assignment lvals exprs) = do
    sequence_ $ zipWith assigner lvals exprs
  where
    assigner (LVar lval val = do
-}

execReturnStatement :: [AST.Expr] -> Closure -> LuaM Bubble
execReturnStatement exprs cls = do
    vals <- map head <$> mapM (\e -> eval e cls) exprs
    return $ ReturnBubble vals
