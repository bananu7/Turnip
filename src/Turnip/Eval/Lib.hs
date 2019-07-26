{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib (loadBaseLibrary) where

import Turnip.Eval.Types
import Turnip.Eval.TH
import Turnip.Eval.Util
import Turnip.Eval.Eval (callRef, call)
import Turnip.Eval.Metatables
import Control.Monad.Except

import Numeric (showGFloat)

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

luarawset :: NativeFunction
luarawset (Table tr : k : v : _) = setTableField tr (k,v) >> return [Table tr]
luarawset _ = throwErrorStr "Invalid rawset parameters"

decimalDigits :: Double -> Maybe Int
decimalDigits x = if x == (fromIntegral . (floor :: Double -> Int) $ x) then Just 0 else Nothing

luatostring :: NativeFunction
luatostring (Nil : _) = return [Str "nil"]
luatostring (Table tr : _) = do
    mt <- getMetatable (Table tr)
    case mt of
        Just mtr -> do
            toString <- getTableField mtr (Str "__tostring")
            call toString [(Table tr)]
        Nothing -> return [Str $ "table: " ++ show tr]

luatostring (Function fr : _) = return [Str $ "function: " ++ show fr]
luatostring (Str s : _) = return [Str s]
luatostring (Boolean True : _) = return [Str "true"]
luatostring (Boolean False : _) = return [Str "false"]
luatostring (Number n : _) = return [Str $ showGFloat (decimalDigits n) n ""]
luatostring _ = throwErrorStr "Wrong argument to 'tostring', value expected"

isInt :: Double -> Bool
isInt x = x == (fromIntegral ((floor :: Double -> Int) x))

toInt :: Double -> Maybe Int
toInt x = if isInt x then Just . floor $ x else Nothing

decimalDigits :: Double -> Maybe Int
decimalDigits x = if isInt x then Just 0 else Nothing

luatonumber :: NativeFunction
luatonumber (Number n : []) = return [Number n]
luatonumber (Str s : []) = return [Number $ read s]
luatonumber (Str s : Number base : _) = readNumberBase base s
luatonumber (_ : _) = return [Nil]
luatonumber _ = throwErrorStr "Wrong argument to 'tonumber'"
    where
        readNumberBase base s =
            if not $ isInt base then
                throwErrorStr "Wrong argument #2 to 'tonumber' (base must be an integer)"
            else
                let maybeVal = foldl (accumDigit ((floor :: Double -> Int) $ xbase) (Just 0) s
                return $ [maybe Nil (Number . fromIntegral)]

        accumDigit :: Int -> Maybe Int -> Char -> Maybe Int
        accumDigit base val digit = do
            if isHexDigit digit then
                Just $ val + digitToInt * base
            else do
                let lowerDigit = toLower digit
                if ord lowerDigit >= ord 'g' && ord lowerDigit <= ord 'z' then
                    Just ... -- don'd do that, use digitToInt impl end extend to base 36
                else
                    Nothing
        isInt x = x == fromIntegral (round x)

luatype :: NativeFunction
luatype (Nil : _) = return [Str "nil"]
luatype (Table _ : _) = return [Str "table"]
luatype (Function _ : _) = return [Str "function"]
luatype (Str _ : _) = return [Str "string"]
luatype (Boolean _ : _) = return [Str "boolean"]
luatype (Number _ : _) = return [Str "number"]
luatype _ = throwErrorStr "Wrong argument to 'type', value expected"

luaselect :: NativeFunction
luaselect (Str "#" : args) = return [Number . fromIntegral . length $ args]
-- luaselect (Str n : args) = return [args !! luatonuber n] -- cursed implicit coercion version
-- it works in the original Lua but I'm not implementing this.
luaselect (Number n : args) = 
    case toInt n of
        Just i -> return $ drop (i-1) args
        Nothing -> throwErrorStr "Wrong argument to select (number has no integer representation)"
luaselect _ = throwErrorStr "Wrong argument to select, either number or string '#' required."

loadBaseLibrary :: LuaM ()
loadBaseLibrary = do
    loadBaseLibraryGen

    addNativeFunction "error" (BuiltinFunction luaerror)
    addNativeFunction "pcall" (BuiltinFunction luapcall)

    addNativeFunction "getmetatable" (BuiltinFunction luagetmetatable)
    addNativeFunction "setmetatable" (BuiltinFunction luasetmetatable)
    addNativeFunction "rawset" (BuiltinFunction luarawset)
    addNativeFunction "tostring" (BuiltinFunction luatostring)
    addNativeFunction "tonumber" (BuiltinFunction luatonumber)
    addNativeFunction "type" (BuiltinFunction luatype)
    addNativeFunction "select" (BuiltinFunction luaselect)
