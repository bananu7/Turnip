{-# LANGUAGE TemplateHaskell #-}

module Eval.TH (genDecls, genLibLoadFunction, entry, TypeT(..), Sig(..)) where

import qualified Eval.Types as Eval
import Eval.Util

import Language.Haskell.TH

import Control.Applicative
import Control.Monad
import Data.Typeable
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
genLibLoadFunction xs = do
    let stmts = mapM toAddFunctionStatement xs
    let body = stmts >>= return . DoE    

    [d|
        loadBaseLibraryGen :: Eval.LuaM ()
        loadBaseLibraryGen = $( body )
        |]
    where
        toAddFunctionStatement :: Entry -> Q Stmt
        toAddFunctionStatement (_, luaName, tempName, _) = noBindS $ [e| addNativeFunction $(litE $ stringL luaName) (Eval.BuiltinFunction $(varE tempName)) |]

-- |Generates a declaration of a function compatible with Lua interface
-- @param tempName - the new name
genDec :: Sig -> Name -> Name -> Q [Dec]
genDec (Sig paramTs returnT) tempName origName = do
    matches <- mapM typeToMatch paramTs

    let
      match :: [Q Pat]
      -- to accept more args, it'd need to use ConP '(:) [m, WildP]
      -- but I'm too lazy to write it right now (TODO)
      match = [return . ListP $ (map fst $ matches)]

    let
      params :: Q [Exp]
      params = mapM (varE . snd) matches

    let
      app :: Q Exp
      app = foldl AppE (VarE origName) <$> params 

    -- this rather convoluted body just means to use appropriate type wrapper for the function's
    -- return type
    let body = normalB $ [| return $ [ $(appE (conE $ typeToName returnT) app) ] |]

    -- Generate the function body and a signature for it
    sigQ <- sigD tempName [t| Eval.NativeFunction |]
    bodyQ <- funD tempName [clause match body []]

    return [sigQ, bodyQ]
