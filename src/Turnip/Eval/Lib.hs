{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib (loadBaseLibrary) where

import Turnip.Eval.Types
import Turnip.Eval.TH
import Turnip.Eval.Util
import Turnip.Eval.Eval (callRef)
import Control.Monad.Except
import Control.Lens ((^.), at)

-- math helpers
deg x = x / pi * 180

$(do
    entries <- sequence [
        entry (Sig [NumberT, NumberT] NumberT) "/" '(/)

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
luasetmetatable (Table tr : Nil : _) = setMetatable tr Nothing >> return [Nil] -- reset to nil
luasetmetatable (Table tr : Table mtr : _) = setMetatable tr (Just mtr) >> return [Nil]
luasetmetatable _ = throwErrorStr "Wrong parameters to setmetatable"

luagetmetatable :: NativeFunction
luagetmetatable (Table tr : _) = do
    mt <- (^. metatable) <$> getTableData tr
    case mt of
        Nothing -> return [Nil]
        Just tr -> return [Table tr]
luagetmetatable _ = throwErrorStr "Wrong argument to luagetmetatable, table expected"

{-
  https://www.lua.org/pil/13.1.html
  To choose a metamethod, Lua does the following:
    (1) If the first value has a metatable with an __add field, Lua uses this value as the metamethod,
        independently of the second value;
    (2) otherwise, if the second value has a metatable with an __add field, Lua uses this value as the metamethod;
    (3) otherwise, Lua raises an error.

    __add, __mul, __sub (for subtraction), __div (for division),
    __unm (for negation), and __pow
-}

getMetaFunction :: String -> Value -> LuaM (Maybe FunctionRef)
getMetaFunction fstr v = do
    mtr <- getMetatable v
    case mtr of
        Just tr -> do
            f <- (^. mapData . at (Str fstr)) <$> getTableData tr
            case f of
                Just (Function fr) -> return $ Just fr
                _                  -> return Nothing
        Nothing -> return Nothing

luametaop :: String -> NativeFunction
luametaop fstr (a : b : _) = do
    maybeFn <- getMetaFunction fstr a
    case maybeFn of
        Just fra -> callRef fra [a,b]
        _ -> do
            maybeFnB <- getMetaFunction fstr b
            case maybeFnB of
                Just frb -> callRef frb [a,b]
                _ -> throwErrorStr "No way to subtract those two values"

luaplus :: NativeFunction
luaplus (Number a : Number b : _) = return $ [Number (a + b)]
luaplus (a : b : _) = luametaop "__add" [a,b]
luaplus _ = throwErrorStr "Plus operator needs at least two values"

luamult :: NativeFunction
luamult (Number a : Number b : _) = return $ [Number (a * b)]
luamult (a : b : _) = luametaop "__mult" [a,b]
luamult _ = throwErrorStr "Mult operator needs at least two values"

luaminus :: NativeFunction
luaminus (Number a : []) = return $ [Number (-a)] --unary negate
luaminus (a : []) = return $ undefined -- todo: __unm

luaminus (Number a : Number b : _) = return $ [Number (a - b)]
luaminus (a : b : _) = luametaop "__sub" [a,b]
luaminus _ = throwErrorStr "Can't subtract those things"

loadBaseLibrary :: LuaM ()
loadBaseLibrary = do
    loadBaseLibraryGen
    addNativeFunction "==" (BuiltinFunction luaCmpEQ)
    addNativeFunction ">" (BuiltinFunction luaCmpGT)
    addNativeFunction "<" (BuiltinFunction luaCmpLT)

    addNativeFunction "-" (BuiltinFunction luaminus)
    addNativeFunction "+" (BuiltinFunction luaplus)
    addNativeFunction "*" (BuiltinFunction luamult)

    addNativeFunction "not" (BuiltinFunction luaNot)
    addNativeFunction "or" (BuiltinFunction luaOr)
    addNativeFunction "and" (BuiltinFunction luaAnd)

    addNativeFunction "error" (BuiltinFunction luaerror)
    addNativeFunction "pcall" (BuiltinFunction luapcall)

    addNativeFunction "getmetatable" (BuiltinFunction luagetmetatable)
    addNativeFunction "setmetatable" (BuiltinFunction luasetmetatable)
