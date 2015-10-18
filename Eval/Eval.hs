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
    result <- execBlock block topCls
    return result

eval :: AST.Expr -> LuaM [Value]
eval (AST.Number n) = return [Number n]
eval (AST.StringLiteral _ str) = return [Str str]
eval (AST.Bool b) = return [Boolean b]
eval AST.Nil = return [Nil]
eval AST.Ellipsis = throwError "how do you even eval ellipsis"

-- lambda needs to be stored in the function table
eval (AST.Lambda argNames b) = do
    _G <- getGlobalTable
    return [Nil]

eval (AST.Var name) = do
    _G <- getGlobalTable

    let mVal = Map.lookup (Str name) _G
    case mVal of
        Just val -> return [val]
        Nothing -> return [Nil]

eval (AST.Call fn args) = do
    -- theoretically always a Nil should be returned, but
    -- it's not in the type system yet. Big TODO on [Values]
    -- custom type - perhaps called ValuePack?
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

execBlock :: AST.Block -> TableRef -> LuaM [Value]
execBlock (AST.Block stmts) cls = do
    mVals <- forM stmts $ \stmt -> do
        case stmt of
            -- the only statement that return values is Return
            AST.Return exprs -> do
                eitherResult <- execReturnStatement exprs
                case eitherResult of
                    Right res -> return $ Just res
                    Left _ -> return (Nothing :: Maybe [Value])
            _ -> do
                maybeError <- execStmt stmt cls
                return Nothing

    let vals = catMaybes mVals

    case vals of
        (h:_) -> return h
        [] -> return [Nil]

        -- if it's a break statement and we're in the loop, we should stop processing this block immediately.
        -- a break statement outside of the loop should be thrown out at pre-analysis stage, so keeping
        -- the "in loop" state isn't necessary

        -- if it's a return statement the block execution should be stopped as well, and the value returned

        -- if any of the functions raises an error, execution should be stopped

execStmt :: AST.Stmt -> TableRef -> LuaM (Maybe LuaError)

execStmt (AST.If bs elseB) cls = do
    forM_ bs $ \(expr, b) -> do
        result <- coerceToBool <$> eval expr
        if result
          then do
            _ <- execBlock b cls
            return ()
          else
            return ()

    return Nothing
        -- for every elseif block
        -- eval its condition and exec if met

    -- if "else" block is present, exec it if everything else fails

--execStmt (For ...) = do
    -- assign values to cls
    -- while the condition is met
    -- exec block
    -- change the local value of index in cls and go to beginning

execStmt (AST.While e b) cls = do
    result <- coerceToBool <$> eval e
    if result then do
        execStmt (AST.While e b) cls
        return Nothing
    else
        return Nothing
    -- if no change has been made to lua state, it can be safely assumed that it's
    --- an infinite loop

-- call statement is a naked call expression with result ignored
execStmt (AST.CallStmt f ps) cls = do
    _ <- eval (AST.Call f ps)
    return Nothing

execReturnStatement :: [AST.Expr] -> LuaM (Either LuaError [Value])
execReturnStatement exprs = do
    vals <- map head <$> mapM eval exprs
    return $ Right vals
