{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Eval.Lib (loadBaseLibrary) where

import Eval.Types
import Eval.TH
import Eval.Util
import Control.Monad.Except

-- math helpers
deg x = x / pi * 180

$(do
    entries <- sequence [
        entry (Sig [NumberT, NumberT] NumberT) "+" '(+)
        -- minus sign needs a helper, see below
        ,entry (Sig [NumberT, NumberT] NumberT) "*" '(*)
        ,entry (Sig [NumberT, NumberT] NumberT) "/" '(/)

        -- math
        ,entry (Sig [NumberT] NumberT) "math.abs" 'abs
        ,entry (Sig [NumberT] NumberT) "math.acos" 'acos
        ,entry (Sig [NumberT] NumberT) "math.asin" 'asin
        ,entry (Sig [NumberT] NumberT) "math.atan" 'asin
        ,entry (Sig [NumberT, NumberT] NumberT) "math.atan2" 'atan2
        --,entry (Sig [NumberT] NumberT) "math.ceil" 'ceiling
        ,entry (Sig [NumberT] NumberT) "math.cos" 'cos
        ,entry (Sig [NumberT] NumberT) "math.cosh" 'cosh
        ,entry (Sig [NumberT] NumberT) "math.deg" 'deg
        ,entry (Sig [NumberT] NumberT) "math.exp" 'exp
        --,entry (Sig [NumberT] NumberT) "math.floor" 'floor
        --,entry (Sig [NumberT] NumberT) "math.fmod" 'fmod
        --,entry (Sig [NumberT] NumberT) "math.frexp" 'frexp
        ]

    temps <- genDecls entries
    loadLib <- genLibLoadFunction entries

    return $ temps ++ loadLib
 )

-- Polymorphic comparison operators
luaCmpEQ (Number a : Number b : _) = return [Boolean $ a == b]
luaCmpEQ (Str a : Str b : _) = return [Boolean $ a == b]
luaCmpEQ (Boolean a : Boolean b : _) = return [Boolean $ a == b]
luaCmpEQ (Str a : Str b : _) = return [Boolean $ a == b]
luaCmpEQ (Nil : Nil : _) = return [Boolean True]
luaCmpEQ _ = return [Boolean False]

luaCmpGT (Number a : Number b : _) = return [Boolean $ a > b]
luaCmpGT (Str a : Str b : _) = return [Boolean $ a > b]
luaCmpGT xs = throwError "Can't compare those values"

luaCmpLT (Number a : Number b : _) = return [Boolean $ a < b]
luaCmpLT (Str a : Str b : _) = return [Boolean $ a < b]
luaCmpLT _ = throwError "Can't compare those values"

luaerror [Str err] = throwError err
luaerror _ = throwError ""

--unary negate
luaMinusHelper (Number a : []) = return $ [Number (-a)]
luaMinusHelper (Number a : Number b : _) = return $ [Number (a - b)]
luaMinusHelper _ = throwError "Can't subtract those things"

loadBaseLibrary :: LuaM ()
loadBaseLibrary = do
    loadBaseLibraryGen
    addNativeFunction "==" (BuiltinFunction luaCmpEQ)
    addNativeFunction ">" (BuiltinFunction luaCmpGT)
    addNativeFunction "<" (BuiltinFunction luaCmpLT)
    addNativeFunction "-" (BuiltinFunction luaMinusHelper)
    addNativeFunction "error" (BuiltinFunction luaerror)