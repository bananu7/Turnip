{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Turnip.Eval (run, runWith, defaultCtx, Context(), Value(..)) where

import qualified Turnip.AST as AST
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Lens hiding (Context)
import Control.Monad.Except

import Turnip.Eval.Types
import Turnip.Eval.Eval
import Turnip.Eval.Lib (loadBaseLibrary)
import Turnip.Eval.Util (throwErrorStr)

runLuaMTWith :: Monad m => Closure -> Context -> LuaMT m a -> m (Either Value a, Context)
runLuaMTWith c s (LuaMT f) = stripWriter <$> runRWST (runExceptT f) c s
    where
        stripWriter (r, c', _w) = (r, c')

-- This is for when you don't care about the closure (want to run code globally)
runLuaMT :: Monad m => Context -> LuaMT m a -> m (Either Value a, Context)
runLuaMT = runLuaMTWith []

-- Context isn't under Either because it's always modified up to
-- the point where the error happened.
runWithM :: Monad m => Context -> AST.Block -> m (Either Value [Value], Context)
runWithM ctx b = runLuaMT ctx (blockRunner b)

-- helper for pure usage
runWith :: Context -> AST.Block -> (Either Value [Value], Context)
runWith ctx b = runIdentity $ runWithM ctx b

-- In this case Either is used both explicitely (with lift)
-- and implicitly (with its MonadError instance)
blockRunner :: forall m. Monad m => AST.Block -> LuaMT m [Value]
blockRunner b = do
    loadBaseLibrary
    result <- execBlock b
    case result of
        ReturnBubble vs -> return vs
        _ -> throwErrorStr "The block didn't end with a returned result"

runM :: forall m. Monad m => AST.Block -> m (Either Value [Value])
runM b = fst <$> runWithM defaultCtx b

-- helper for pure usage
run :: AST.Block -> Either Value [Value]
run b = runIdentity $ runM b

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
    gTableData = Map.fromList [(Str "_G", Table gTableRef)]
