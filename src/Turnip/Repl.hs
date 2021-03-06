{-# LANGUAGE FlexibleContexts #-}

module Turnip.Repl where

import Turnip.Parser
import Turnip.Eval
import Control.Monad.State
import System.IO

import Paths_Turnip (version)
import Data.Version (showVersion)

disableBuffering :: IO ()
disableBuffering = hSetBuffering stdout NoBuffering

repl :: IO ()
repl = do
    disableBuffering 
    putStrLn $ "Turnip REPL v" ++ showVersion version ++ "\n"

    (flip evalStateT) defaultCtx $ forever $ do
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

