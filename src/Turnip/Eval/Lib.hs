{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Turnip.Eval.Lib (loadBaseLibrary) where

import Turnip.Eval.Types
import Turnip.Eval.TH
import Turnip.Eval.Util
import Turnip.Eval.UtilNumbers
import Turnip.Eval.Eval (callRef, callFunction, call)
import Turnip.Eval.Metatables
import qualified Turnip.Parser as Parser
import Control.Monad.Except
import Data.Maybe (fromMaybe)

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

luaassert :: NativeFunction
luaassert (a : msg : _) = if not $ coerceToBool [a] then luaerror [msg] else return [Nil]
luaassert (a : []) = if not $ coerceToBool [a] then luaerror [Str "assertion failed!"] else return [Nil]
luaassert _ = throwErrorStr "Bad argument to 'assert': value expected"

luapcall :: NativeFunction
luapcall (Function fref : args) = ((callRef fref args) >>= prependTrue) `catchError` pcallHandler
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

luarawget :: NativeFunction
luarawget (Table tr : k : _) = (:[]) . fromMaybe Nil <$> rawGetTableField tr k
luarawget _ = throwErrorStr "Invalid rawget parameters"

luarawlen :: NativeFunction
luarawlen (Str s : _) = return [Number . fromIntegral . length $ s]
luarawlen (Table tr : _) = (:[]) <$> getTableLength tr
luarawlen _ = throwErrorStr "Invalid rawget parameters"

luarawequal :: NativeFunction
luarawequal (Nil : Nil : _) = return [Boolean False]
luarawequal (a : b : _) = return [Boolean $ a == b]
luarawequal _ = throwErrorStr "rawequal needs at least two parameters" 

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


readNumberBase' :: Double -> String -> LuaM [Value]
readNumberBase' base s =
    case toInt base of
        Just basei -> return $ [readNumberBase basei s]
        Nothing -> throwErrorStr "Wrong argument #2 to 'tonumber' (base must be an integer)"


luatonumber :: NativeFunction
luatonumber (Number n : []) = return [Number n]
luatonumber (Str s : []) = return $ [readNumberBase 10 s]
luatonumber (Str s : Number base : _) = readNumberBase' base s
luatonumber (_ : _) = return [Nil]
luatonumber _ = throwErrorStr "Wrong argument to 'tonumber'"


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

luaload :: NativeFunction -- ld, source, mode, env
luaload (Str src : _) = loadstring src
luaload (Function f : _) = do
    fd <- getFunctionData f
    src <- getChunkSource fd ""
    loadstring src

    where
        getChunkSource fd src = do
            chunkPiece <- callFunction fd []
            case chunkPiece of
                -- https://www.lua.org/manual/5.4/manual.html#pdf-load
                -- "Each call to ld must return a string that concatenates with previous results.
                -- A return of an empty string, nil, or no value signals the end of the chunk."
                []       -> return src
                [Str ""] -> return src
                [Nil]    -> return src
                [Str s]  -> getChunkSource fd (src ++ s)
                _ -> throwErrorStr "Function passed to 'load' didn't return a string."

luaload _ = throwErrorStr "Wrong argument to loadstring, string required."

loadstring :: String -> LuaM [Value]
loadstring src = do
    b <- case Parser.parseLua src of
        Right block -> return block
        Left err -> throwErrorStr $ "Parse error: " ++ show err
    f <- makeNewLambda (FunctionData [] b [] False)
    return [Function f]

luanext :: NativeFunction
luanext (Table tr : _)       = do
    (k,v) <- getFirstTableField tr
    return [k, v]
luanext (Table tr : Nil : _) = do
    (k,v) <- getFirstTableField tr
    return [k, v]
luanext (Table tr : k : _)   = do
    p <- getNextTableField tr k
    case p of
        Just (Nil, Nil) -> return [Nil]
        Just (k,v) -> return [k, v]
        Nothing -> throwErrorStr "Wrong argument no 'next', invalid key"
    
luanext _ = throwErrorStr "Wrong argument to 'next', table [and key] required."

luapairs :: NativeFunction
luapairs (Table tr : _) = error "Not implemented yet - luapairs" -- call next or someshit
luapairs _ = throwErrorStr "Wrong argument to 'pairs', table required."

loadBaseLibrary :: LuaM ()
loadBaseLibrary = do
    loadBaseLibraryGen

    addNativeFunction "error" (BuiltinFunction luaerror)
    addNativeFunction "assert" (BuiltinFunction luaassert)
    addNativeFunction "pcall" (BuiltinFunction luapcall)

    addNativeFunction "getmetatable" (BuiltinFunction luagetmetatable)
    addNativeFunction "setmetatable" (BuiltinFunction luasetmetatable)

    addNativeFunction "rawset" (BuiltinFunction luarawset)
    addNativeFunction "rawget" (BuiltinFunction luarawget)
    addNativeFunction "rawlen" (BuiltinFunction luarawlen)
    addNativeFunction "rawequal" (BuiltinFunction luarawequal)

    addNativeFunction "tostring" (BuiltinFunction luatostring)
    addNativeFunction "tonumber" (BuiltinFunction luatonumber)
    addNativeFunction "type" (BuiltinFunction luatype)
    addNativeFunction "select" (BuiltinFunction luaselect)

    -- loadstring has been removed in 5.2
    addNativeFunction "load" (BuiltinFunction luaload)
