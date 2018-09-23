{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Turnip.Eval.Types where

import qualified Turnip.AST as AST (Block)

import qualified Data.Map as Map
import Control.Lens hiding (Context)
import Control.Monad.Except
import Control.Monad.RWS

-- TODO - think about reference counting on those
newtype TableRef = TableRef Int deriving (Ord, Eq, Show)
newtype FunctionRef = FunctionRef Int deriving (Ord, Eq, Show)

newtype LuaMT m a = LuaMT (ExceptT Value (RWST Closure () Context m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError Value)

-- MonadState Context is not provided on purpose

-- potentially I could provide those, but the user can also add them himself
-- MonadCont, MonadReader Closure, MonadWriter (if ever)

instance MonadTrans LuaMT where
    lift = LuaMT . lift . lift

type LuaM a = forall m. Monad m => LuaMT m a

data Value where {
    Nil :: Value;
    Table :: TableRef -> Value;
    Function :: FunctionRef -> Value;
    Str :: String -> Value;
    Boolean :: Bool -> Value;
    Number :: Double -> Value;
    } deriving (Ord, Eq, Show)

-- I don't think it's supposed to be an existential
type NativeFunction = [Value] -> LuaM [Value]

-- This is a stack of tables forming a stack of nested closures

data ClosureLevel = ClosureLevel { closureTableRef :: TableRef, closureVarargs :: Maybe [Value] }
type Closure = [ClosureLevel]

data FunctionData = 
      FunctionData {
        closure :: Closure,
        functionDataBlock :: AST.Block,
        paramNames :: [String],
        varargs :: Bool
      }
    | BuiltinFunction {
        nativeFn :: NativeFunction
      }

type TableMapData = Map.Map Value Value
data TableData = TableData {
    _mapData :: TableMapData,
    _metatable :: Maybe TableRef -- Can be either nil or some table
    }

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

makeLenses ''TableData