{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib (loadBaseLibrary) where

import Turnip.Eval.Types
import Turnip.Eval.TH
import Turnip.Eval.Util
import Turnip.Eval.Eval (callRef)
import Control.Monad.Except
import Control.Lens ((^.))

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
luaCmpGT xs = throwErrorStr "Can't compare those values"

luaCmpLT :: NativeFunction
luaCmpLT (Number a : Number b : _) = return [Boolean $ a < b]
luaCmpLT (Str a : Str b : _) = return [Boolean $ a < b]
luaCmpLT _ = throwErrorStr "Can't compare those values"

-- Bool-coercing logical operators
luaNot, luaOr, luaAnd :: NativeFunction

-- Not is an unary operator
luaNot [a] = return [Boolean . not . coerceToBool $ [a]]
luaNot _ = throwErrorStr "Lua not operator must be called on one value!"

luaOr [a,b] = return [Boolean $ (coerceToBool [a]) || (coerceToBool [b])]
luaOr _ = throwErrorStr "'or' must be called on two values"

luaAnd [a,b] = return [Boolean $ (coerceToBool [a]) && (coerceToBool [b])]
luaAnd _ = throwErrorStr "'and' must be called on two values"

luaerror :: NativeFunction
luaerror (a:_) = throwError a
luaerror _ = throwError Nil

luapcall :: NativeFunction
luapcall (Function fref : _) = ((callRef fref []) >>= prependTrue) `catchError` pcallHandler
    where
        prependTrue result = return $ Boolean True : result
        pcallHandler e = return [Boolean False, e]
luapcall (_a : _) = return [Boolean False, Str "Attempt to call something that isn't a function"]
luapcall _ = throwErrorStr "Bad argument to 'pcall': value expected"

luasetmetatable :: NativeFunction
luasetmetatable (Table tr : Nil : _) = undefined -- reset to nil
luasetmetatable (Table tr : Table mtr : _) = undefined
luasetmetatable _ = throwErrorStr "Wrong parameters to setmetatable"

luagetmetatable :: NativeFunction
luagetmetatable (Table tr : _) = do
    mt <- (^. metatable) <$> getTableData tr
    case mt of
        Nothing -> return [Nil]
        Just tr -> return [Table tr]
luagetmetatable _ = throwErrorStr "Wrong argument to luagetmetatable, table expected"

--unary negate
luaMinusHelper :: NativeFunction
luaMinusHelper (Number a : []) = return $ [Number (-a)]
luaMinusHelper (Number a : Number b : _) = return $ [Number (a - b)]
luaMinusHelper _ = throwErrorStr "Can't subtract those things"

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
    addNativeFunction "pcall" (BuiltinFunction luapcall)

    addNativeFunction "getmetatable" (BuiltinFunction luagetmetatable)
    addNativeFunction "setmetatable" (BuiltinFunction luasetmetatable)
