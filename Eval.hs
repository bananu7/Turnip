{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Eval (run, runWith, defaultCtx, Context(), Value(..)) where

import Prelude hiding (Nil)

import qualified LuaAS as AST
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Control.Lens hiding (Context)

import Eval.Types
import Eval.Eval
import Eval.Util
import Eval.Lib (loadBaseLibrary)

runWith :: AST.Block -> Context -> ([Value], Context)
runWith b ctx = runState code ctx
    where
        globalTableRef = ctx ^. gRef
        code = do
            loadBaseLibrary
            execBlock b globalTableRef

run :: AST.Block -> [Value]
run b = fst $ runWith b defaultCtx

defaultCtx :: Context
defaultCtx = Context {
    _gRef = gRef,
    _functions = Map.fromList [],
    _tables = Map.fromList [(gRef, Map.fromList [])],
    _lastId = 1
    }
  where
    gRef = TableRef 999
