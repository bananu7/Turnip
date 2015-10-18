{-# LANGUAGE FlexibleContexts #-}

module Repl where

import Parser
import Eval
import Control.Monad.State
import System.IO

disableBuffering = hSetBuffering stdout NoBuffering

repl :: IO ()
repl = do
    disableBuffering 
    (flip evalStateT) defaultCtx $ forever $ do
        line <- liftIO $ do
            putStr "> "
            getLine

        let maybeAST = parseLua line

        case maybeAST of
            Right ast -> do
                ctx <- get
                maybeResult <- state $ runWith ast
                case maybeResult of 
                    Right result -> liftIO $ print result
                    Left error -> liftIO . putStrLn $ "Lua error " ++ show error

            Left error ->
                liftIO . putStrLn $ "Parse error " ++ show error

