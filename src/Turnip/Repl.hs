{-# LANGUAGE FlexibleContexts #-}

module Turnip.Repl where

import Turnip.Parser
import Turnip.Eval
import Control.Monad.State
import System.IO

import Paths_Turnip (version)
import Data.Version (showVersion)

import qualified Turnip.AST as AST
import Text.ParserCombinators.Parsec (Parser, ParseError, parse, (<|>))

data ReplConfig = ReplConfig
  { file             :: String
  , interactive      :: Bool
  }

data ReplBlock = ReplBlock AST.Block | ReplExpr [AST.Expr]

replBlock :: Parser ReplBlock
replBlock = (ReplBlock . AST.Block <$> block) <|> (ReplExpr <$> explist)

parseLuaRepl :: String -> Either ParseError ReplBlock
parseLuaRepl = parse replBlock ""

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

repl :: ReplConfig -> IO ()
repl cfg = do
    let ctx = defaultCtx
    disableBuffering 
    putStrLn $ "Turnip REPL v" ++ showVersion version ++ "\n"

    ctx' <- case file cfg of
                "" -> return ctx
                filePath -> runFileFromCommandline filePath ctx

    (flip evalStateT) ctx' $ forever $ do
        line <- liftIO $ do
            putStr "> "
            getLine

        let maybeAST = parseLuaRepl line

        case maybeAST of
            Right (ReplBlock b) -> do
                maybeResult <- state $ \s -> runWith s b
                printResult maybeResult
            Right (ReplExpr xs) -> do
                maybeResult <- mapM_ (\x -> state $ \s -> evalWith s x) xs
                printResult maybeResult
            Left err ->
                liftIO . putStrLn $ "Parse error " ++ show err
    where
        printResult (Right result) = liftIO $ print result
        printResult (Left err) = liftIO . putStrLn $ "Lua error " ++ show err

