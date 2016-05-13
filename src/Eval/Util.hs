{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Eval.Util where

import Eval.Types
import Control.Lens
import qualified Data.Map as Map (lookup, empty)
import Control.Applicative ((<$>))
import Data.Map

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
coerceToBool (Boolean x:_) = x
coerceToBool (Nil:_) = False
coerceToBool (h:_) = True
coerceToBool _ = False

getGlobalTable :: LuaM TableData
getGlobalTable = do
    gref <- use gRef
    -- assume that _G is always present (as it should)
    (Just _G) <- Map.lookup gref <$> use tables
    return _G

addNativeFunction :: String -> FunctionData -> LuaM ()
addNativeFunction name fdata = do
    newRef <- uniqueFunctionRef
    functions . at newRef .= Just fdata

    gTabRef <- use gRef
    tables . at gTabRef . traversed . at (Str name) .= Just (Function newRef)

makeNewTableWith :: TableData -> LuaM TableRef
makeNewTableWith initial = do
    newRef <- uniqueTableRef
    tables . at newRef .= Just initial
    return newRef

makeNewTable :: LuaM TableRef
makeNewTable = makeNewTableWith Map.empty

setTableField :: TableRef -> (Value, Value) -> LuaM ()
setTableField tRef (k,v) = tables . at tRef . traversed %= insert k v

getTableField :: TableRef -> Value -> LuaM Value
getTableField tRef k = getTableData tRef >>= \t -> case t ^. at k of
    Just v -> return v
    Nothing -> return Nil