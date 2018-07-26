{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib (loadBaseLibrary) where

import Turnip.Eval.Types
import Turnip.Eval.TH
import Turnip.Eval.Util
import Turnip.Eval.Eval (callRef)
import Turnip.Eval.Metatables
import Control.Monad.Except
import Control.Lens ((^.), at)
import Data.Map (lookupMax)

-- math helpers
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
luaCmpGT (a : b : _) = luametaop "__lt" [b,a] -- order reversed
luaCmpGT xs = throwErrorStr "Can't compare those values"

luaCmpLT :: NativeFunction
luaCmpLT (Number a : Number b : _) = return [Boolean $ a < b]
luaCmpLT (Str a : Str b : _) = return [Boolean $ a < b]
luaCmpLT (a : b : _) = luametaop "__lt" [a,b]
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
luagetmetatable (t : _) = do
    mt <- getMetatable t
    case mt of
        Just mtr -> do
            metatableHider <- rawGetTableField mtr (Str "__metatable")
            case metatableHider of
                Just mth -> return [mth]
                Nothing -> return [Table mtr]
        Nothing -> return [Nil]
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

luametaop :: String -> NativeFunction
luametaop fstr (a : b : _) = do
    maybeFn <- getMetaFunction fstr a
    case maybeFn of
        Just fra -> callRef fra [a,b]
        _ -> do
            maybeFnB <- getMetaFunction fstr b
            case maybeFnB of
                Just frb -> callRef frb [a,b]
                _ -> throwErrorStr $ "No metaop '" ++ fstr ++ "' on those two values"

luametaop fstr [a] = do
    maybeFn <- getMetaFunction fstr a
    case maybeFn of
        Just fr -> callRef fr [a]
        _ -> throwErrorStr $ "No metaop '" ++ fstr ++ "' on this value"

luametaop _ _ = throwErrorStr $ "Invalid metaop call" -- should really never happen

luarawset :: NativeFunction
luarawset (Table tr : k : v : _) = setTableField tr (k,v) >> return [Table tr]
luarawset _ = throwErrorStr "Invalid rawset parameters"

luaplus :: NativeFunction
luaplus (Number a : Number b : _) = return $ [Number (a + b)]
luaplus (a : b : _) = luametaop "__add" [a,b]
luaplus _ = throwErrorStr "Plus operator needs at least two values"

luamult :: NativeFunction
luamult (Number a : Number b : _) = return $ [Number (a * b)]
luamult (a : b : _) = luametaop "__mult" [a,b]
luamult _ = throwErrorStr "Mult operator needs at least two values"

luadiv :: NativeFunction
luadiv (Number a : Number b : _) = return $ [Number (a / b)]
luadiv (a : b : _) = luametaop "__div" [a,b]
luadiv _ = throwErrorStr "Div operator needs at least two values"

luaminus :: NativeFunction
luaminus (Number a : []) = return $ [Number (-a)] --unary negate
luaminus (a : []) = luametaop "__unm" [a]
luaminus [] = throwErrorStr "Minus operator called on 0 arguments"

luaminus (Number a : Number b : _) = return $ [Number (a - b)]
luaminus (a : b : _) = luametaop "__sub" [a,b]
luaminus _ = throwErrorStr "Can't subtract those things"

luaconcat :: NativeFunction
luaconcat (Str a : Str b : _) = return [Str $ a ++ b]
luaconcat (a : b : _) = luametaop "__concat" [a,b]
luaconcat _ = throwErrorStr "Concat operator needs at least two values"

lualen :: NativeFunction
lualen (Str a : _) = return [Number . fromIntegral $ length a]
lualen (Table tr : _) = do
    (TableData td _) <- getTableData tr
    case lookupMax td of
        Just (Number x, _) -> return [Number x]
        _ -> return [Number 0]
lualen (a : _) = luametaop "__len" [a]
lualen [] = throwErrorStr "Length operator called on 0 arguments"
        
loadBaseLibrary :: LuaM ()
loadBaseLibrary = do
    loadBaseLibraryGen
    addNativeFunction "==" (BuiltinFunction luaCmpEQ)
    addNativeFunction ">" (BuiltinFunction luaCmpGT)
    addNativeFunction "<" (BuiltinFunction luaCmpLT)

    addNativeFunction "#" (BuiltinFunction lualen)

    addNativeFunction "-" (BuiltinFunction luaminus)
    addNativeFunction "+" (BuiltinFunction luaplus)
    addNativeFunction "*" (BuiltinFunction luamult)
    addNativeFunction "/" (BuiltinFunction luadiv)

    addNativeFunction ".." (BuiltinFunction luaconcat)

    addNativeFunction "not" (BuiltinFunction luaNot)
    addNativeFunction "or" (BuiltinFunction luaOr)
    addNativeFunction "and" (BuiltinFunction luaAnd)

    addNativeFunction "error" (BuiltinFunction luaerror)
    addNativeFunction "pcall" (BuiltinFunction luapcall)

    addNativeFunction "getmetatable" (BuiltinFunction luagetmetatable)
    addNativeFunction "setmetatable" (BuiltinFunction luasetmetatable)
    addNativeFunction "rawset" (BuiltinFunction luarawset)
