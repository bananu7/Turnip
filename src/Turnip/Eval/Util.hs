{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Turnip.Eval.Util where

import Turnip.Eval.Types
import Control.Lens
import qualified Data.Map as Map (lookup, empty, lookupMax, split, findMin, notMember, null)
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

setGlobal :: Value -> Value -> LuaM ()
setGlobal k v = LuaMT $ do
    gTabRef <- use gRef
    tables . at gTabRef . traversed . mapData . at k .= Just v

createNativeFunction :: FunctionData -> LuaM FunctionRef
createNativeFunction fdata = do
    newRef <- uniqueFunctionRef
    LuaMT $ functions . at newRef .= Just fdata
    return newRef

addNativeFunction :: String -> FunctionData -> LuaM FunctionRef
addNativeFunction name fdata = do
    newRef <- createNativeFunction fdata
    setGlobal (Str name) (Function newRef)
    return newRef

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
setTableField tr (k,v) = LuaMT $ tables . at tr . traversed . mapData %= insert k v

rawGetTableField :: TableRef -> Value -> LuaM (Maybe Value)
rawGetTableField tr k = (^. mapData . at k) <$> getTableData tr

getTableField :: TableRef -> Value -> LuaM Value
getTableField tr k = maybe Nil id <$> rawGetTableField tr k

getFirstTableField :: TableRef -> LuaM (Value, Value)
getFirstTableField tr = do
    md <- (^. mapData) <$> getTableData tr
    if Map.null md
        then return (Nil, Nil)
        else return (Map.findMin md)

-- This is a Maybe Value because Nil is a legitimate result, while
-- Nothing means that the passed key doesn't exist and errors out;
-- this mirrors the behavior of the original next.
getNextTableField :: TableRef -> Value -> LuaM (Maybe (Value, Value))
getNextTableField tr k = do
    md <- (^. mapData) <$> getTableData tr
    if Map.notMember k md then
        return Nothing
    else
        let (_, right) = Map.split k md in
        return . Just $ if Map.null right
            then (Nil, Nil)
            else Map.findMin right

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
