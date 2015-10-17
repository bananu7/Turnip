{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval.Lib (loadBaseLibrary) where

import Eval.Types
import Eval.TH
import Eval.Util

luaOpPlus :: NativeFunction
luaOpPlus ((Number a):(Number b):_) = return $ [Number (a + b)]
luaOpPlus _ = error "Plus operator takes exactly two numeric arguments"

$(gen (Sig [NumberT, NumberT] NumberT) "luaOpMinus" '(-))

loadBaseLibrary :: LuaM ()
loadBaseLibrary = do
    addNativeFunction "+" (BuiltinFunction [] luaOpPlus)
    addNativeFunction "-" (BuiltinFunction [] luaOpMinus)