{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Turnip.Eval.Util where

import Turnip.Eval.Types
import Control.Lens
import qualified Data.Map as Map (lookup, empty, lookupMax)
import Data.Map
import Control.Monad.Except (throwError)

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
coerceToBool (Number n:_) = not $ isNaN n
coerceToBool (_h:_) = True
coerceToBool _ = False

-- This function fills in the Nil for the missing Value
extractVal :: Maybe Value -> Value
extractVal Nothing = Nil
extractVal (Just v) = v

-- This was needed to shield Eval from seeing into the LuaM insides
getGlobalTableRef :: LuaM TableRef
getGlobalTableRef = LuaMT $ use gRef

addNativeFunction :: String -> FunctionData -> LuaM ()
addNativeFunction name fdata = do
    newRef <- uniqueFunctionRef
    LuaMT $ do
        functions . at newRef .= Just fdata

        gTabRef <- use gRef
        tables . at gTabRef . traversed . mapData . at (Str name) .= Just (Function newRef)

makeNewTableWith :: TableMapData -> LuaM TableRef
makeNewTableWith initial = do
    newRef <- uniqueTableRef
    LuaMT $ do
        tables . at newRef .= Just (TableData initial Nothing)
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
setTableField _    (Nil, _) = throwErrorStr "Table index is nil"
setTableField tRef (k,v) = LuaMT $ tables . at tRef . traversed . mapData %= insert k v

rawGetTableField :: TableRef -> Value -> LuaM (Maybe Value)
rawGetTableField tRef k = (^. mapData . at k) <$> getTableData tRef

rawGetTableFieldByIndex :: TableRef -> Int -> LuaM (Maybe (Value, Value))
rawGetTableFieldByIndex tRef i = (^. mapData . elemAt i) <$> getTableData tRef

getTableField :: TableRef -> Value -> LuaM Value
getTableField tr k = maybe const Nil <$> rawGetTableField tr k

getTableFieldByIndex :: TableRef -> Int -> LuaM (Value, Value)
getTableFieldByIndex tr i = maybe const (Nil, Nil) <$> rawGetTableFieldByIndex

getTableLength :: TableRef -> LuaM Value
getTableLength tr = do
    (TableData td _) <- getTableData tr
    case Map.lookupMax td of
        Just (Number x, _) -> return $ Number x
        _ -> return $ Number 0

throwErrorStr :: String -> LuaM a
throwErrorStr = throwError . Str

-- |This function is currently not doing anything over regular throwError.
-- In the future, however, it should be used to report critical vm errors (such as totality problems etc.)
vmErrorStr :: String -> LuaM a
vmErrorStr = throwErrorStr
