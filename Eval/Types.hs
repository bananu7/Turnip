{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval.Types where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Lens hiding (Context)
import qualified LuaAS as AST (Block)
import Control.Applicative
import Control.Monad.Except

newtype TableRef = TableRef Int deriving (Ord, Eq, Show)
newtype FunctionRef = FunctionRef Int deriving (Ord, Eq, Show)

-- Applicative is here just for 7.8 (I know, right)
type LuaM a = forall m . (MonadState Context m, Applicative m, MonadError String m) => m a

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

data FunctionData = FunctionData { closure :: TableData, block :: AST.Block, paramNames :: [String] }
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
