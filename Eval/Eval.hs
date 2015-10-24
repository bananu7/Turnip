{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval.Eval where

import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import qualified LuaAS as AST
import Eval.Types
import Eval.Util

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.Except

call :: FunctionData -> [Value] -> LuaM [Value]
call (BuiltinFunction fn) args = do
    -- ensure args match signature
    -- if yes, extract them into list of Haskell values ? what about Haskell functions operating on Lua-level values ?
    result <- fn args
    -- call the fn and take its result
    -- possibly with liftIO
    return result

call (FunctionData cls topCls block names) args = do
    -- for every arg set cls[names[i]] = args[i]
    -- in case of a (trailing) vararg function, set `arg` variable to hold
    -- (the REST of) the arguments
    cl <- makeNewTable
    sequence_ $ zipWith (setArg cl) names args
    res <- execBlock block cl
    case res of
        ReturnBubble vs -> return vs
        _ -> return [Nil]

  where
    setArg :: TableRef -> String -> Value -> LuaM ()
    setArg cl n v = setTableField cl (Str n, v)


eval :: AST.Expr -> LuaM [Value]
eval (AST.Number n) = return [Number n]
eval (AST.StringLiteral _ str) = return [Str str]
eval (AST.Bool b) = return [Boolean b]
eval AST.Nil = return [Nil]
eval AST.Ellipsis = throwError "how do you even eval ellipsis"

-- lambda needs to be stored in the function table
eval (AST.Lambda parNames b) = do
    g <- use gRef
    newRef <- uniqueFunctionRef
    functions . at newRef .= (Just $ FunctionData (Map.fromList []) g b parNames)
    return [Function newRef]

eval (AST.Var name) = do
    _G <- getGlobalTable

    let mVal = Map.lookup (Str name) _G
    case mVal of
        Just val -> return [val]
        Nothing -> return [Nil]

eval (AST.Call fn args) = do
    -- theoretically always a Nil should be returned, but
    -- it's not in the type system yet. (wrt head)
    argVs <- map head <$> mapM eval args
    fnV <- eval fn

    case fnV of 
        (Function ref:_) -> do
            fData <- getFunctionData ref
            call fData argVs
        x -> throwError $ "Trying to call something that doesn't eval to a function! (" ++ show x ++ ")"

eval (AST.FieldRef t k) = do
    tv <- eval t

    -- we ignore any values returned by the expression because
    -- we only want to index the first one anyway
    case head tv of
        (Table tRef) -> do
            -- similarly here
            kV <- head <$> eval k

            t <- getTableData tRef
            let mVal :: Maybe Value = t ^. at kV

            case mVal of
                Just v -> return [v]
                Nothing -> return [Nil]

        _ -> throwError "Trying to index a non-table"


-- this is essentially the same as regular call
-- TODO should it even be a difference in the AST?
eval (AST.BinOp name lhs rhs) = eval (AST.Call (AST.Var name) [lhs, rhs])
eval (AST.UnOp name expr) = eval (AST.Call (AST.Var name) [expr])

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

execBlock :: AST.Block -> TableRef -> LuaM Bubble
execBlock (AST.Block stmts) cls = runUntil stmts $ \stmt -> do
    case stmt of
        -- the only statement that return values is Return
        AST.Return exprs -> execReturnStatement exprs cls
        _ -> execStmt stmt cls

execStmt :: AST.Stmt -> TableRef -> LuaM Bubble

execStmt (AST.If blocks mElseB) cls = do
    -- if an else block is present, we can append it to the list
    -- with a predicate that always evals to True.
    let blocks' = case mElseB of
                    Just elseB -> blocks ++ [(AST.Bool True, elseB)]
                    Nothing -> blocks

    runUntil blocks' $ \(expr, b) -> do
        result <- coerceToBool <$> eval expr
        if result
          then execBlock b cls
          else return EmptyBubble

--execStmt (For ...) = do
    -- assign values to cls
    -- while the condition is met
    -- exec block
    -- change the local value of index in cls and go to beginning

execStmt (AST.While e b) cls = do
    result <- coerceToBool <$> eval e
    if result then
        execStmt (AST.While e b) cls
    else
        return EmptyBubble
    -- if no change has been made to lua state, it can be safely assumed that it's
    -- an infinite loop

-- call statement is a naked call expression with result ignored
execStmt (AST.CallStmt f ps) cls = do
    _ <- eval (AST.Call f ps)
    return EmptyBubble


-- this is a special case of an unpacking assignment
execStmt (AST.Assignment lvals [expr]) cls = do
    vals <- eval expr
    -- fill in the missing Nil-s for zip
    let valsPadded = vals ++ replicate (length lvals - length vals) (Nil)

    sequence_ $ zipWith assignLValue lvals vals
    return EmptyBubble


assignLValue :: AST.LValue -> Value -> LuaM ()
assignLValue (AST.LVar name) v = do
    g <- use gRef
    setTableField g (Str name, v)

assignLValue (AST.LFieldRef {}) v = error "Assignment of fieldrefs not implemented"

{-
executionStmt (AST.Assignment lvals exprs) = do
    sequence_ $ zipWith assigner lvals exprs
  where
    assigner (LVar lval val = do
-}

execReturnStatement :: [AST.Expr] -> LuaM Bubble
execReturnStatement exprs = do
    vals <- map head <$> mapM eval exprs
    return $ ReturnBubble vals
