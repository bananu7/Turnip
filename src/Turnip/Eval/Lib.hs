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

    addNativeFunction "error" (BuiltinFunction luaerror)
    addNativeFunction "pcall" (BuiltinFunction luapcall)

    addNativeFunction "getmetatable" (BuiltinFunction luagetmetatable)
    addNativeFunction "setmetatable" (BuiltinFunction luasetmetatable)
    addNativeFunction "rawset" (BuiltinFunction luarawset)
