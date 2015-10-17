{-# LANGUAGE TemplateHaskell #-}

module Eval.TH where

import qualified Eval.Types as Eval

import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Data.Typeable

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

-- |Generates a declaration of a function compatible with Lua interface
gen :: Sig -> String -> Name -> Q [Dec]
gen (Sig paramTs returnT) n f = do
    let fn = mkName n
    xn <- newName "xs"

    matches <- mapM typeToMatch paramTs

    let
      match :: [Q Pat]
      -- to accept more args, it'd need to use ConP '(:) [m, WildP]
      -- but I'm too lazy to write it right now
      match = [return . ListP $ (map fst $ matches)]

    let
      params :: Q [Exp]
      params = mapM (varE . snd) matches

    let
      app :: Q Exp
      app = foldl AppE (VarE f) <$> params 

    -- this rather convoluted body just means to use appropriate type wrapper for the function's
    -- return type
    let body = normalB $ [| return $ [ $(appE (conE $ typeToName returnT) app) ] |]

    (:[]) <$> funD fn [clause match body []]
