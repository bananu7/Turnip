{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval.Types where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Lens hiding (Context)
import qualified LuaAS as AST (Block)
import Control.Applicative
import Control.Monad.Except

-- TODO - think about reference counting on those
newtype TableRef = TableRef Int deriving (Ord, Eq, Show)
newtype FunctionRef = FunctionRef Int deriving (Ord, Eq, Show)

type EvalContext = (Context, Closure)

newtype LuaMT m a = LuaMT (ExceptT String (StateT EvalContext m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

-- MonadState EvalContext is not provided on purpose

-- potentially I could provide those, but the user can also add them himself
-- MonadCont, MonadReader r, MonadWriter w

instance MonadTrans LuaMT where
    lift = LuaMT . lift . lift

type LuaM a = forall m. Monad m => LuaMT m a

data Value where {
    Table :: TableRef -> Value;
    Function :: FunctionRef -> Value;
    Str :: String -> Value;
    Number :: Double -> Value;
    Boolean :: Bool -> Value;
    Nil :: Value;
    } deriving (Ord, Eq, Show)

type TableData = Map.Map Value Value

-- I don't think it's supposed to be an existential
type NativeFunction = [Value] -> LuaM [Value]

-- This is a stack of tables forming a stack of nested closures
type Closure = [TableRef]

data FunctionData = FunctionData { closure :: Closure, block :: AST.Block, paramNames :: [String] }
                  | BuiltinFunction { fn :: NativeFunction }

data Context = Context {
    _gRef :: TableRef,
    _functions :: Map.Map FunctionRef FunctionData,
    _tables :: Map.Map TableRef TableData,
    _lastId :: Int
    }
makeLenses ''Context

type LuaError = String

-- |This type represents something that breaks the block execution
-- and moves up the statement chain.
data Bubble = BreakBubble | ReturnBubble [Value] | EmptyBubble deriving (Show, Eq)
