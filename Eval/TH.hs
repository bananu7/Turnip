{-# LANGUAGE TemplateHaskell #-}

module Eval.TH where

import Eval.Types

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Data.Typeable

gen :: [String] -> String -> Name -> Q [Dec]
gen ts n f = do
    let fn = mkName n
    xn <- newName "xs"

    matches <- mapM typeToMatch ts

    let
      match :: [Q Pat]
      match = [return . ListP . map fst $ matches]

    let
      params :: Q [Exp]
      params = mapM (varE . snd) matches

    let
      app :: Q Exp
      app = foldl AppE (VarE f) <$> params 

    let body = normalB $ [| return $ [ Number $(app) ] |]

    (:[]) <$> funD fn [clause match body []]

-- |Transforms a constructor Name into a pattern match for a newly introduced name
toPatName :: Name -> Q (Pat, Name) 
toPatName p = do
    name <- newName "x"
    liftM2 (,) (conP p [varP name]) (pure name)

-- |Transforms a type information string into a pattern for that with a anme
typeToMatch :: String -> Q (Pat, Name)
typeToMatch t = case t of
    "Int" -> toPatName 'Number
    "String" -> toPatName 'Str
    "Bool" -> toPatName 'Boolean




