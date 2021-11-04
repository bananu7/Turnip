{-# LANGUAGE TemplateHaskell #-}

module Turnip.Eval.TH (genDecls, genLibLoadFunction, entry, TypeT(..), Sig(..)) where

import qualified Turnip.Eval.Types as Eval
import Turnip.Eval.Util

import Language.Haskell.TH

import Control.Monad
import Data.Char

-- |Function signature has n params and one return value
data Sig = Sig [TypeT] TypeT

data TypeT = NumberT | StringT | BooleanT

-- |Transforms a type information value into a data construct name
typeToName :: TypeT -> Name
typeToName t = case t of
    NumberT -> 'Eval.Number
    StringT -> 'Eval.Str
    BooleanT -> 'Eval.Boolean

-- |Transforms a constructor Name into a pattern match for a newly introduced name
toPatName :: Name -> Q (Pat, Name) 
toPatName p = do
    name <- newName "x"
    liftM2 (,) (conP p [varP name]) (pure name)

typeToMatch :: TypeT -> Q (Pat, Name) 
typeToMatch = toPatName . typeToName

type Entry = (Sig, String, Name, Name)

entry :: Sig -> String -> Name -> Q Entry
entry sig luaName origName = do
    tempName <- newName ("lua" ++ toSafeSuffix luaName)
    return (sig, luaName, tempName, origName)
    where
        toSafeSuffix = concat . map (show . ord)
        
genDecls :: [Entry] -> Q [Dec]
genDecls es = concat <$> mapM (\(sig, _, tempName, origName) -> genDec sig tempName origName) es

genLibLoadFunction :: [Entry] -> Q [Dec]
genLibLoadFunction entries = do
    let funs = ListE <$> mapM toModuleItem entries

    [d|
        loadBaseLibraryGen :: String -> Eval.LuaM ()
        loadBaseLibraryGen modName = addNativeModule modName $(funs)
        |]
    where
        toModuleItem :: Entry -> Q Exp
        toModuleItem (_, luaName, tempName, _) = [e| (luaName, Eval.BuiltinFunction $(varE tempName)) |]


-- |Generates a declaration of a function compatible with Lua interface
-- @param tempName - the new name
genDec :: Sig -> Name -> Name -> Q [Dec]
genDec (Sig paramTs returnT) tempName origName = do
    matches <- mapM typeToMatch paramTs

    let listOfPatterns = map fst matches
    let
      -- the generated function accepts one param, with a pattern of (t1 : t2 : t3 : ... : _),
      -- where t1..tn are the types in its signature
      inputPattern :: Q Pat
      inputPattern = return $ foldr (\par pat -> ConP '(:) [par, pat]) WildP listOfPatterns

    let
      params :: Q [Exp]
      params = mapM (varE . snd) matches

    let
      app :: Q Exp
      app = foldl AppE (VarE origName) <$> params

    -- this rather convoluted body just means to use appropriate type wrapper for the function's
    -- return type
    let body = normalB [| return $ [ $(appE (conE $ typeToName returnT) app) ] |]

    -- error body is used when the static signature doesn't match the passed arguments
    -- TODO: maybe it could be useful to allow writing native functions accepting any value
    -- or generally being more flexible in their arguments; I'll leave that for the future
    -- public API.
    let errorBody = normalB [| throwErrorStr "The arguments are wrong for this native function." |]

    -- Generate the function body and a signature for it
    sigDec <- sigD tempName [t| Eval.NativeFunction |]
    bodyDec <-
      funD tempName [
        clause [inputPattern] body [],
        clause [wildP] errorBody []
        ]

    return [sigDec, bodyDec]
