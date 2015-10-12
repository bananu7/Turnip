{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval (run, Value(..)) where

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
import Eval.TH

luaOpPlus :: NativeFunction
luaOpPlus ((Number a):(Number b):_) = return $ [Number (a + b)]
luaOpPlus _ = error "Plus operator takes exactly two numeric arguments"

$(gen ["Int", "Int"] "luaOpMinus" '(-))

runWith :: AST.Block -> Context -> [Value]
runWith b ctx = evalState code ctx
    where
        globalTableRef = ctx ^. gRef
        code = do
            addNativeFunction "+" (BuiltinFunction [] luaOpPlus)
            execBlock b globalTableRef

run :: AST.Block -> [Value]
run b = runWith b defaultCtx

defaultCtx :: Context
defaultCtx = Context {
    _gRef = gRef,
    _functions = Map.fromList [],
    _tables = Map.fromList [(gRef, Map.fromList [])],
    _lastId = 1
    }
  where
    gRef = TableRef 999

addNativeFunction :: String -> FunctionData -> LuaM ()
addNativeFunction name fdata = do
    newRef <- uniqueFunctionRef
    functions . at newRef .= Just fdata

    gTabRef <- use gRef
    tables . at gTabRef . traversed . at (Str name) .= Just (Function newRef)
