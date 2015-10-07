{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Eval where

import Prelude hiding (Nil)

import qualified LuaAS as AST
import qualified Data.Map as Map
import Control.Monad.State

newtype TableRef = TableRef Int deriving (Ord, Eq)
newtype FunctionRef = FunctionRef Int deriving (Ord, Eq)

data Value where
    Table :: TableRef -> Value
    Function :: FunctionRef -> Value
    Str :: String -> Value
    Number :: Double -> Value
    Boolean :: Bool -> Value
    Nil :: Value

newtype TableData = TableData (Map.Map Value Value)

type NativeType = String -- todo
type NativeSignature = [NativeType]

-- I don't think it's supposed to be an existential
type NativeFunction = [Value] -> LuaM [Value]

data FunctionData = FunctionData { closure :: TableData, topLevelClosure :: TableRef, block :: AST.Block, paramNames :: [String] }
                  | BuiltinFunction { signature :: NativeSignature, fn :: NativeFunction }

data Context = Context {
    _G :: Map.Map String Value,
    functions :: Map.Map FunctionRef FunctionData,
    tables :: Map.Map TableRef TableData
    }

----------------------

type LuaM a = forall m . MonadState Context m => m a

type LuaError = String

getFunctionData :: FunctionRef -> LuaM FunctionData
getFunctionData ref = do
    fns <- gets functions
    case Map.lookup ref fns of
        Just fdata -> return fdata
        Nothing -> error "Function ref dead" -- should never really happen

getTableData :: TableRef -> LuaM TableData
getTableData ref = do
    ts <- gets tables
    case Map.lookup ref ts of
        Just tdata -> return tdata
        Nothing -> error "Function ref dead" -- should never really happen

coerceToBool :: [Value] -> Bool
coerceToBool (h:_) = True
coerceToBool _ = False

call :: Value -> [Value] -> LuaM [Value]
call (Function ref) args = getFunctionData ref >>= ((flip call') args)
call _ _ = error "You called something that isn't a function"

call' :: FunctionData -> [Value] -> LuaM [Value]
call' (BuiltinFunction sig fn) args = do
    -- ensure args match signature
    -- if yes, extract them into list of Haskell values ? what about Haskell functions operating on Lua-level values ?
    result <- fn args
    -- call the fn and take its result
    -- possibly with liftIO
    return result

call' (FunctionData cls topCls block names) args = do
    -- for every arg set cls[names[i]] = args[i]
    -- in case of a (trailing) vararg function, set `arg` variable to hold
    -- (the REST of) the arguments
    result <- execBlock block topCls
    return result

execBlock :: AST.Block -> TableRef -> LuaM [Value]
execBlock (AST.Block stmts) cls = do
    forM_ stmts $ \stmt -> do
        maybeResult <- execStmt stmt cls
        return Nil
    return [Nil]
        -- the only statement that return values is Return
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
    -- check the value in cls
    -- exec block
    -- go to beginning

    -- if no change has been made to lua state, it can be safely assumed that it's
    --- an infinite loop

execReturnStatement :: AST.Stmt -> LuaM (Either LuaError [Value])
execReturnStatement s = return $ Right [Nil]

eval :: AST.Expr -> LuaM [Value]
eval (AST.Bool b) = return [Boolean b]
eval (AST.Number n) = return [Number n]
eval AST.Nil = return [Nil]

