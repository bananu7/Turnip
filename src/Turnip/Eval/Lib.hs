{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib (loadBaseLibrary) where

import Turnip.Eval.Types
import Turnip.Eval.TH
import Turnip.Eval.Util
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
luaCmpEQ :: NativeFunction
luaCmpEQ (Number a : Number b : _) = return [Boolean $ a == b]
luaCmpEQ (Str a : Str b : _) = return [Boolean $ a == b]
luaCmpEQ (Boolean a : Boolean b : _) = return [Boolean $ a == b]
luaCmpEQ (Nil : Nil : _) = return [Boolean True]
luaCmpEQ _ = return [Boolean False]

luaCmpGT :: NativeFunction
luaCmpGT (Number a : Number b : _) = return [Boolean $ a > b]
luaCmpGT (Str a : Str b : _) = return [Boolean $ a > b]
luaCmpGT xs = throwError "Can't compare those values"

luaCmpLT :: NativeFunction
luaCmpLT (Number a : Number b : _) = return [Boolean $ a < b]
luaCmpLT (Str a : Str b : _) = return [Boolean $ a < b]
luaCmpLT _ = throwError "Can't compare those values"

-- Bool-coercing logical operators
luaNot, luaOr, luaAnd :: NativeFunction

-- Not is an unary operator
luaNot [a] = return [Boolean . not . coerceToBool $ [a]]
luaNot _ = throwError "Lua not operator must be called on one value!"

luaOr [a,b] = return [Boolean $ (coerceToBool [a]) || (coerceToBool [b])]
luaOr _ = throwError "'or' must be called on two values"

luaAnd [a,b] = return [Boolean $ (coerceToBool [a]) && (coerceToBool [b])]
luaAnd _ = throwError "'and' must be called on two values"

luaerror :: NativeFunction
luaerror [Str err] = throwError err
luaerror _ = throwError ""

--unary negate
luaMinusHelper :: NativeFunction
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

    addNativeFunction "not" (BuiltinFunction luaNot)
    addNativeFunction "or" (BuiltinFunction luaOr)
    addNativeFunction "and" (BuiltinFunction luaAnd)

    addNativeFunction "error" (BuiltinFunction luaerror)