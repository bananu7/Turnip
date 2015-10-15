{-# LANGUAGE TemplateHaskell #-}

module Eval.TH where

import Eval.Types

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Data.Typeable
    
genFn :: String -> Q [Dec]
genFn n = do
    fn <- newName n
    xn <- newName "x"
    
    let fakeSig = ["ell", "puppy"]
    let stmts :: [StmtQ]; stmts = map (\s -> noBindS $ [e| print s |]) fakeSig
    
    let body = normalB . doE $ stmts
    (:[]) <$> funD fn [clause [varP xn] body []]

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

toPatName :: Name -> Q (Pat, Name) 
toPatName p = do
    name <- newName "x"
    liftM2 (,) (conP p [varP name]) (pure name)

typeToMatch :: String -> Q (Pat, Name)
typeToMatch t = case t of
    "Int" -> toPatName 'Number
    "String" -> toPatName 'Str
    "Bool" -> toPatName 'Boolean




