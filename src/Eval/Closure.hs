{-# LANGUAGE RankNTypes, FlexibleContexts #-}

-- |This module contains functions that simplify (and hide) closure handling
-- inside of the LuaM monad
module Eval.Closure 
    (getClosure
    ,closureLookup
    ,closurePush
    ,assignmentTarget
    )
    where

import Eval.Types
import Eval.Util
import Control.Lens
import qualified Data.Map as Map (lookup)
import Control.Monad.Trans
import qualified LuaAS as AST

-- |Get the raw closure value.
getClosure :: LuaM Closure
getClosure = LuaMT . lift $ use _2

-- |Index the current closure.
closureLookup :: Value -> LuaM Value
closureLookup v = getClosure >>= closureLookupFrom v

closureLookupFrom :: Value -> Closure -> LuaM Value
-- descend recursively with lookups, picking the closest name first
closureLookupFrom v (topRef:cls) = do
    topCls <- getTableData topRef
    case Map.lookup v topCls of
        Just val -> return val
        Nothing -> closureLookupFrom v cls
-- if closure lookup fails, try global lookup
closureLookupFrom v _ = do  
    _G <- getGlobalTable
    let mVal = Map.lookup v _G
    return $ extractVal mVal

-- |Executes the code block one level deeper.
closurePush :: forall m a. Monad m => TableRef -> LuaMT m a -> LuaMT m a
closurePush t a = do
    LuaMT . lift $ _2 %= (:) t
    r <- a
    LuaMT . lift $ _2 %= tail
    return r

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
    t <- getTableData headCls
    case Map.lookup (Str name) t of
        -- if the name appears in the closure, we assign to this one
        (Just _) -> return headCls
        -- otherwise we try going down the stack
        Nothing -> assignmentTargetHelper restCls name
