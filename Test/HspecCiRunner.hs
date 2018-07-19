{-# LANGUAGE ScopedTypeVariables #-}

module HspecCiRunner (hspecCi) where

import Test.Hspec
import XmlTestFormatter (xmlFormatter)
import Test.Hspec.Runner
import System.Environment (getEnv)
import Test.Hspec.Formatters (specdoc)
import Control.Exception

testResultsPath :: String
testResultsPath = "test-results/"

hspecCi :: String -> Spec -> IO ()
hspecCi filename spec = do
    isCiBuild <- (== "true") <$> getEnv "CI" `catch` \(e :: SomeException) -> return ""

    let ciConfig = defaultConfig
                 { configFormatter = Just xmlFormatter
                 , configOutputFile = Right $ testResultsPath ++ filename ++ "/results.xml"
                 }

    hspecWith (if isCiBuild then ciConfig else defaultConfig) spec