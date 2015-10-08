{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Prelude hiding (Nil)

import qualified LuaAS as AST
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Control.Lens hiding (Context)

newtype TableRef = TableRef Int deriving (Ord, Eq, Show)
newtype FunctionRef = FunctionRef Int deriving (Ord, Eq, Show)

type LuaM a = forall m . MonadState Context m => m a

data Value where {
    Table :: TableRef -> Value;
    Function :: FunctionRef -> Value;
    Str :: String -> Value;
    Number :: Double -> Value;
    Boolean :: Bool -> Value;
    Nil :: Value;
    } deriving (Ord, Eq, Show)

type TableData = Map.Map Value Value

type NativeType = String -- todo
type NativeSignature = [NativeType]

-- I don't think it's supposed to be an existential
type NativeFunction = [Value] -> LuaM [Value]

data FunctionData = FunctionData { closure :: TableData, topLevelClosure :: TableRef, block :: AST.Block, paramNames :: [String] }
                  | BuiltinFunction { signature :: NativeSignature, fn :: NativeFunction }

data Context = Context {
    _gRef :: TableRef,
    _functions :: Map.Map FunctionRef FunctionData,
    _tables :: Map.Map TableRef TableData,
    _lastId :: Int
    }
makeLenses ''Context

----------------------

type LuaError = String

getFunctionData :: FunctionRef -> LuaM FunctionData
getFunctionData ref = do
    fns <- use functions
    case Map.lookup ref fns of
        Just fdata -> return fdata
        Nothing -> error "Function ref dead" -- should never really happen

getTableData :: TableRef -> LuaM TableData
getTableData ref = do
    ts <- use tables
    case Map.lookup ref ts of
        Just tdata -> return tdata
        Nothing -> error "Function ref dead" -- should never really happen

uniqueFunctionRef :: LuaM FunctionRef
uniqueFunctionRef = do
    lastId += 1
    FunctionRef <$> use lastId

uniqueTableRef :: LuaM TableRef
uniqueTableRef = do
    lastId += 1
    TableRef <$> use lastId

coerceToBool :: [Value] -> Bool
coerceToBool (h:_) = True
coerceToBool _ = False

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

execReturnStatement :: [AST.Expr] -> LuaM (Either LuaError [Value])
execReturnStatement exprs = do
    vals <- map head <$> mapM eval exprs
    return $ Right vals

getGlobalTable :: LuaM TableData
getGlobalTable = do
    gref <- use gRef
    -- assume that _G is always present (as it should)
    (Just _G) <- Map.lookup gref <$> use tables
    return _G

eval :: AST.Expr -> LuaM [Value]

eval (AST.Number n) = return [Number n]
eval (AST.StringLiteral _ str) = return [Str str]
eval (AST.Bool b) = return [Boolean b]
eval AST.Nil = return [Nil]
eval AST.Ellipsis = error "how do you even eval ellipsis"

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
        x -> error $ "Trying to call something that doesn't eval to a function! (" ++ show x ++ ")"

-- this is essentially the same as regular call
-- TODO should it even be a difference in the AST?
eval (AST.BinOp name lhs rhs) = eval (AST.Call (AST.Var name) [lhs, rhs])
eval (AST.UnOp name expr) = eval (AST.Call (AST.Var name) [expr])

call :: FunctionData -> [Value] -> LuaM [Value]
call (BuiltinFunction sig fn) args = do
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

-------------------------------

luaOpPlus :: NativeFunction
luaOpPlus ((Number a):(Number b):_) = return $ [Number (a + b)]
luaOpPlus _ = error "Plus operator takes exactly two numeric arguments"

runWith :: AST.Block -> Context -> [Value]
runWith b ctx = evalState code ctx
    where
        globalTableRef = ctx ^. gRef
        code = do
            addNativeFunction "+" (BuiltinFunction [] luaOpPlus)
            execBlock b globalTableRef

run :: AST.Block -> [Value]
run b = runWith b defaultCtx

defaultCtx :: Context
defaultCtx = Context {
    _gRef = gRef,
    _functions = Map.fromList [],
    _tables = Map.fromList [(gRef, Map.fromList [])],
    _lastId = 1
    }
  where
    gRef = TableRef 999

addNativeFunction :: String -> FunctionData -> LuaM ()
addNativeFunction name fdata = do
    newRef <- uniqueFunctionRef
    functions . at newRef .= Just fdata

    gTabRef <- use gRef
    tables . at gTabRef . traversed . at (Str name) .= Just (Function newRef)
