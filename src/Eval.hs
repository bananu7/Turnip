{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Eval (run, runWith, defaultCtx, Context(), Value(..)) where

import Prelude hiding (Nil)

import qualified LuaAS as AST
import qualified Data.Map as Map
import Control.Monad.State
import Control.Lens hiding (Context)
import Control.Monad.Except

import Eval.Types
import Eval.Eval
import Eval.Lib (loadBaseLibrary)

runLuaMTWith :: EvalContext -> LuaMT m a -> m (Either String a, EvalContext)
runLuaMTWith s (LuaMT f) = runStateT (runExceptT f) s

-- This is for when you don't care about the closure (want to run code globally)
runLuaMT :: forall a m. Monad m => Context -> LuaMT m a -> m (Either String a, Context)
runLuaMT ctx f = do 
        -- This destructures EvalContext assuming that it's a simple tuple
    (x, (ctx', _)) <- runLuaMTWith (ctx, []) f
    return (x, ctx')

-- Context isn't under Either because it's always modified up to
-- the point where the error happened.
runWithM :: forall m. Monad m => Context -> AST.Block -> m (Either String [Value], Context)
runWithM ctx b = runLuaMT ctx (blockRunner b)

-- helper for pure usage
runWith :: Context -> AST.Block -> (Either String [Value], Context)
runWith ctx b = runIdentity $ runWithM ctx b

-- In this case Either is used both explicitely (with lift)
-- and implicitly (with its MonadError instance)
blockRunner :: forall m. Monad m => AST.Block -> LuaMT m [Value]
blockRunner b = do
    loadBaseLibrary
    result <- execBlock b
    case result of
        ReturnBubble vs -> return vs
        _ -> throwError "The block didn't end with a returned result"

runM :: forall m. Monad m => AST.Block -> m (Either String [Value])
runM b = fst <$> runWithM defaultCtx b

-- helper for pure usage
run :: AST.Block -> Either String [Value]
run b = runIdentity $ runM b

defaultCtx :: Context
defaultCtx = Context {
    _gRef = gRef,
    _functions = Map.fromList [],
    _tables = Map.fromList [(gRef, Map.fromList [])],
    _lastId = 10
    }
  where
    gRef = TableRef 1
