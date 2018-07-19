{-# LANGUAGE ScopedTypeVariables #-}

module HspecCiRunner (hspecCi) where

import Test.Hspec
import XmlTestFormatter (xmlFormatter)
import Test.Hspec.Runner
import System.Environment (getEnv)
import Test.Hspec.Formatters (specdoc)
import Control.Exception

hspecCi :: Spec -> IO ()
hspecCi spec = do
    isCiBuild <- (== "CI") <$> getEnv "CI" `catch` \(e :: SomeException) -> return ""

    let ciConfig = defaultConfig
                 { configFormatter = Just xmlFormatter
                 , configOutputFile = Right "results.xml"
                 }

    hspecWith (if isCiBuild then ciConfig else defaultConfig) spec