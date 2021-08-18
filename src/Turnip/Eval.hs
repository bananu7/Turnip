{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Turnip.Eval
    ( runWithDefault
    , runWith
    , runWithM
    , evalWith
    , evalWithM
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

runLuaMTWith :: Monad m => Closure -> LuaMT m a -> Context -> m (Either Value a, Context)
runLuaMTWith cls (LuaMT f) ctx = stripWriter <$> runRWST (runExceptT f) cls ctx
    where
        stripWriter (r, c, _w) = (r, c)

-- This is for when you don't care about the closure (want to run code globally)
runLuaMT :: Monad m => LuaMT m a -> Context -> m (Either Value a, Context)
runLuaMT = runLuaMTWith []

-- Context isn't under Either because it's always modified up to
-- the point where the error happened.
runWithM :: Monad m => AST.Block -> Context -> m (Either Value [Value], Context)
runWithM b = runLuaMT (execBlockResult b) 

-- helper for pure usage
runWith :: AST.Block -> Context -> (Either Value [Value], Context)
runWith b = runIdentity . runWithM b

evalWithM :: Monad m => AST.Expr -> Context -> m (Either Value [Value], Context)
evalWithM e = runLuaMT (eval e)

evalWith :: AST.Expr -> Context -> (Either Value [Value], Context)
evalWith e = runIdentity . evalWithM e

-- Those default runners are just for testing
runWithDefaultM :: forall m. Monad m => AST.Block -> m (Either Value [Value])
runWithDefaultM b = fst <$> runLuaMT (loadBaseLibrary >> execBlockResult b) defaultCtx

runWithDefault :: AST.Block -> Either Value [Value]
runWithDefault b = runIdentity $ runWithDefaultM b

defaultCtx :: Context
defaultCtx = Context {
    _gRef = gTableRef,
    _functions = Map.empty,
    _tables = Map.fromList [(gTableRef, gTable)],
    _lastId = 10
    }
  where
    gTableRef = TableRef 1
    gTable = TableData gTableData Nothing
    gTableData = Map.fromList [
        (Str "_G", Table gTableRef),
        (Str "_VERSION", Str $ "Turnip " ++ showVersion version)
        ]
