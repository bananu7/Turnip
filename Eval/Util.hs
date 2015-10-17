{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Eval.Util where

import Eval.Types
import Control.Lens
import qualified Data.Map as Map (lookup)
import Control.Applicative ((<$>))

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

getGlobalTable :: LuaM TableData
getGlobalTable = do
    gref <- use gRef
    -- assume that _G is always present (as it should)
    (Just _G) <- Map.lookup gref <$> use tables
    return _G
