{-# LANGUAGE FlexibleContexts #-}

module Turnip.Repl where

import Turnip.Parser
import Turnip.Eval
import Control.Monad.State
import System.IO
import System.Environment (getArgs)

import Paths_Turnip (version)
import Data.Version (showVersion)

handleCommandLine :: Context -> IO Context
handleCommandLine ctx = do
    args <- getArgs

    case args of
        (path:_) -> runFileFromCommandline path ctx
        _ -> return ctx

runFileFromCommandline :: String -> Context -> IO Context
runFileFromCommandline path ctx = do
    source <- readFile path
    flip execStateT ctx $ do
        let maybeAST = parseLua source

        case maybeAST of
            Right ast -> do
                maybeResult <- state $ \s -> runWith s ast
                case maybeResult of
                    Right result -> liftIO $ print result
                    Left err -> do
                        liftIO . putStrLn $ "Lua error " ++ show err

            Left err -> do
                liftIO . putStrLn $ "Parse error " ++ show err

disableBuffering :: IO ()
disableBuffering = hSetBuffering stdout NoBuffering

repl :: Context -> IO ()
repl ctx = do
    disableBuffering 
    putStrLn $ "Turnip REPL v" ++ showVersion version ++ "\n"

    (flip evalStateT) ctx $ forever $ do
        line <- liftIO $ do
            putStr "> "
            getLine

        let maybeAST = parseLua line

        case maybeAST of
            Right ast -> do
                maybeResult <- state $ \s -> runWith s ast
                case maybeResult of 
                    Right result -> liftIO $ print result
                    Left err -> liftIO . putStrLn $ "Lua error " ++ show err

            Left err ->
                liftIO . putStrLn $ "Parse error " ++ show err

