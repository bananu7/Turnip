{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Turnip.Eval.Util where

import Turnip.Eval.Types
import Control.Lens
import qualified Data.Map as Map (lookup, empty)
import Control.Applicative ((<$>))
import Data.Map

getFunctionData :: FunctionRef -> LuaM FunctionData
getFunctionData ref = LuaMT $ do
    fns <- use functions
    case Map.lookup ref fns of
        Just fdata -> return fdata
        Nothing -> error "Function ref dead" -- should never really happen

getTableData :: TableRef -> LuaM TableData
getTableData ref = LuaMT $ do
    ts <- use tables
    case Map.lookup ref ts of
        Just tdata -> return tdata
        Nothing -> error "Function ref dead" -- should never really happen

uniqueFunctionRef :: LuaM FunctionRef
uniqueFunctionRef = LuaMT $ do
    lastId += 1
    FunctionRef <$> use lastId

uniqueTableRef :: LuaM TableRef
uniqueTableRef = LuaMT $ do
    lastId += 1
    TableRef <$> use lastId

coerceToBool :: [Value] -> Bool
coerceToBool (Boolean x:_) = x
coerceToBool (Nil:_) = False
coerceToBool (_h:_) = True
coerceToBool _ = False

-- This function fills in the Nil for the missing Value
extractVal :: Maybe Value -> Value
extractVal Nothing = Nil
extractVal (Just v) = v

-- This was needed to shield Eval from seeing into the LuaM insides
getGlobalTableRef :: LuaM TableRef
getGlobalTableRef = LuaMT $ use gRef

getGlobalTable :: LuaM TableData
getGlobalTable = LuaMT $ do
    gref <- use gRef
    -- assume that _G is always present (as it should)
    (Just _G) <- Map.lookup gref <$> use tables
    return _G

addNativeFunction :: String -> FunctionData -> LuaM ()
addNativeFunction name fdata = do
    newRef <- uniqueFunctionRef
    LuaMT $ do
        functions . at newRef .= Just fdata

        gTabRef <- use gRef
        tables . at gTabRef . traversed . at (Str name) .= Just (Function newRef)

makeNewTableWith :: TableData -> LuaM TableRef
makeNewTableWith initial = do
    newRef <- uniqueTableRef
    LuaMT $ do
        tables . at newRef .= Just initial
        return newRef

makeNewTable :: LuaM TableRef
makeNewTable = makeNewTableWith Map.empty

-- this is very similar to makeNewTable, just operates on functions
makeNewLambda :: FunctionData -> LuaM FunctionRef
makeNewLambda f = do
    newRef <- uniqueFunctionRef
    LuaMT $ do
        functions . at newRef .= Just f
        return newRef

setTableField :: TableRef -> (Value, Value) -> LuaM ()
setTableField tRef (k,v) = LuaMT $ tables . at tRef . traversed %= insert k v

getTableField :: TableRef -> Value -> LuaM Value
getTableField tRef k = getTableData tRef >>= \t -> case t ^. at k of
    Just v -> return v
    Nothing -> return Nil