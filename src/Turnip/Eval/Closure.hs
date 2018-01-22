{-# LANGUAGE RankNTypes, FlexibleContexts #-}

-- |This module contains functions that simplify (and hide) closure handling
-- inside of the LuaM monad
module Turnip.Eval.Closure 
    (getClosure
    ,closureLookup
    ,closureLookupEllipsis
    ,closurePush
    ,assignmentTarget
    )
    where

import Turnip.Eval.Types
import Turnip.Eval.Util
import qualified Turnip.AST as AST

import Control.Lens
import qualified Data.Map as Map (lookup)
import Control.Monad.Trans
import Control.Monad.RWS
import Control.Monad.Except

-- |Get the raw closure value.
getClosure :: LuaM Closure
getClosure = LuaMT . lift $ ask

-- |Index the current closure.
closureLookup :: Value -> LuaM Value
closureLookup v = getClosure >>= closureLookupFrom v

closureLookupEllipsis :: LuaM (Maybe [Value])
closureLookupEllipsis = getClosure >>= closureLookupEllipsisFrom

closureLookupFrom :: Value -> Closure -> LuaM Value
-- descend recursively with lookups, picking the closest name first
closureLookupFrom v (top:cls) = do
    topCls <- getTableData (fst top)
    case Map.lookup v topCls of
        Just val -> return val
        Nothing -> closureLookupFrom v cls
-- if closure lookup fails, try global lookup
closureLookupFrom v _ = do  
    _G <- getGlobalTable
    let mVal = Map.lookup v _G
    return $ extractVal mVal

closureLookupEllipsisFrom :: Closure -> LuaM (Maybe [Value])
closureLookupEllipsisFrom (top:cls) = do
    case (snd top) of
        Just val -> return $ Just val
        Nothing -> closureLookupEllipsisFrom cls
closureLookupEllipsisFrom [] = return Nothing

-- |Executes the code block one level deeper.
closurePush :: forall m a. Monad m => (TableRef, Maybe [Value]) -> LuaMT m a -> LuaMT m a
closurePush t (LuaMT a) = LuaMT $ mapExceptT (withRWST (\cls s -> (t:cls, s))) a

-- this is a simple helper that picks either top level closure or global table
assignmentTarget :: AST.Name -> LuaM TableRef
-- before choosing local closure for assignment, we should first check
-- whether the value doesn't exist in the closure
-- this is essentially the core of lexical scoping, I suppose
assignmentTarget name = do
    cls <- getClosure
    assignmentTargetHelper cls name

assignmentTargetHelper :: Closure -> AST.Name -> LuaM TableRef
assignmentTargetHelper [] _ = getGlobalTableRef
assignmentTargetHelper (headCls:restCls) name = do
    t <- getTableData $ fst headCls
    case Map.lookup (Str name) t of
        -- if the name appears in the closure, we assign to this one
        (Just _) -> return $ fst headCls
        -- otherwise we try going down the stack
        Nothing -> assignmentTargetHelper restCls name
