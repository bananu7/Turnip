{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Turnip.Eval
    ( eval
    , execBlockResult
    , runWithDefault
    , runLuaMT
    , defaultCtx
    , Context()
    , Value(..)
    ) where

import qualified Turnip.AST as AST
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Lens hiding (Context)
import Control.Monad.Except

import Turnip.Eval.Types
import Turnip.Eval.Eval
import Turnip.Eval.Lib (loadBaseLibrary)

import Paths_Turnip (version)
import Data.Version (showVersion)

execBlockResult :: forall m. Monad m => AST.Block -> LuaMT m [Value]
execBlockResult b = extractResult <$> execBlock b
    where 
        extractResult (ReturnBubble vs) = vs
        extractResult _ = []

runLuaMTWith :: Monad m => Closure -> LuaMT m a -> Context m -> m (Either Value a, Context m)
runLuaMTWith cls (LuaMT f) ctx = stripWriter <$> runRWST (runExceptT f) cls ctx
    where
        stripWriter (r, c, _w) = (r, c)

-- This is for when you don't care about the closure (want to run code globally)
runLuaMT :: Monad m => LuaMT m a -> Context m -> m (Either Value a, Context m)
runLuaMT = runLuaMTWith []

-- Those default runners are just for testing
runWithDefaultM :: forall m. Monad m => AST.Block -> m (Either Value [Value])
runWithDefaultM b = fst <$> runLuaMT (loadBaseLibrary >> execBlockResult b) defaultCtx

runWithDefault :: AST.Block -> Either Value [Value]
runWithDefault b = runIdentity $ runWithDefaultM b

defaultCtx :: Monad m => Context m
defaultCtx = Context {
    _gRef = gTableRef,
    _functions = Map.empty,
    _tables = Map.fromList [(gTableRef, gTable)],
    _lastId = 10,
    _ioOutCb = (\_ -> return ()),
    _ioInCb = return ""
    }
  where
    gTableRef = TableRef 1
    gTable = TableData gTableData Nothing
    gTableData = Map.fromList [
        (Str "_G", Table gTableRef),
        (Str "_VERSION", Str $ "Turnip " ++ showVersion version)
        ]
