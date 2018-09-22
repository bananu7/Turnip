{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib (loadBaseLibrary) where

import Turnip.Eval.Types
import Turnip.Eval.TH
import Turnip.Eval.Util
import Turnip.Eval.Eval (callRef, binaryMetaOperator, unaryMetaOperator)
import Turnip.Eval.Metatables
import Control.Monad.Except
import Data.Map (lookupMax)
import Data.Maybe (isJust)

-- math helpers
deg :: Floating a => a -> a
deg x = x / pi * 180

$(do
    entries <- sequence [
        -- math
         entry (Sig [NumberT] NumberT) "math.abs" 'abs
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
luaCmpEQ (Nil : Nil : _) = return [Boolean False]
luaCmpEQ (a : b : _)
    | a == b = return [Boolean True]
    | otherwise = luaEQHelper a b
luaCmpEQ _ = throwErrorStr "Comparison requires at least two values"

luaEQHelper :: Value -> Value -> LuaM [Value]
luaEQHelper a b = do
    maybeEqA <- getMetaFunction "__eq" a
    maybeEqB <- getMetaFunction "__eq" b

    case (maybeEqA, maybeEqB) of
        -- meta-equality is only used if both eq functions are the same
        (Just eqA, Just eqB) | eqA == eqB -> callRef eqA [a,b]
        _ -> return [Boolean False]

luaCmpGT :: NativeFunction
luaCmpGT (Number a : Number b : _) = return [Boolean $ a > b]
luaCmpGT (Str a : Str b : _) = return [Boolean $ a > b]
luaCmpGT (a : b : _) = binaryMetaOperator "__lt" [b,a] -- order reversed
luaCmpGT _ = throwErrorStr "Can't compare those values"

luaCmpLT :: NativeFunction
luaCmpLT (Number a : Number b : _) = return [Boolean $ a < b]
luaCmpLT (Str a : Str b : _) = return [Boolean $ a < b]
luaCmpLT (a : b : _) = binaryMetaOperator "__lt" [a,b]
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
luapcall _ = vmErrorStr "Bad argument to 'pcall': value expected"

luasetmetatable :: NativeFunction
luasetmetatable (Table tr : Nil : _) = setMetatable tr Nothing >> return [Nil] -- reset to nil
luasetmetatable (Table tr : Table mtr : _) = setMetatable tr (Just mtr) >> return [Nil]
luasetmetatable _ = vmErrorStr "Wrong parameters to setmetatable"

luagetmetatable :: NativeFunction
luagetmetatable (t : _) = do
    mt <- getMetatable t
    case mt of
        Just mtr -> do
            metatableHider <- rawGetTableField mtr (Str "__metatable")
            case metatableHider of
                Just mth -> return [mth]
                Nothing -> return [Table mtr]
        Nothing -> return [Nil]
luagetmetatable _ = vmErrorStr "Wrong argument to luagetmetatable, table expected"

luarawset :: NativeFunction
luarawset (Table tr : k : v : _) = setTableField tr (k,v) >> return [Table tr]
luarawset _ = throwErrorStr "Invalid rawset parameters"
        
loadBaseLibrary :: LuaM ()
loadBaseLibrary = do
    loadBaseLibraryGen
    addNativeFunction "==" (BuiltinFunction luaCmpEQ)
    addNativeFunction ">" (BuiltinFunction luaCmpGT)
    addNativeFunction "<" (BuiltinFunction luaCmpLT)

    addNativeFunction "not" (BuiltinFunction luaNot)
    addNativeFunction "or" (BuiltinFunction luaOr)
    addNativeFunction "and" (BuiltinFunction luaAnd)

    addNativeFunction "error" (BuiltinFunction luaerror)
    addNativeFunction "pcall" (BuiltinFunction luapcall)

    addNativeFunction "getmetatable" (BuiltinFunction luagetmetatable)
    addNativeFunction "setmetatable" (BuiltinFunction luasetmetatable)
    addNativeFunction "rawset" (BuiltinFunction luarawset)
