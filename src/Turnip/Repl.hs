{-# LANGUAGE FlexibleContexts #-}

module Turnip.Repl where

import Turnip.Parser
import Turnip.Eval
import Turnip.Eval.Lib (loadBaseLibrary)
import Control.Monad.State
import System.IO
import Data.Functor.Identity (runIdentity)

import Paths_Turnip (version)
import Data.Version (showVersion)

import qualified Turnip.AST as AST
import Turnip.PrettyPrint
import Text.ParserCombinators.Parsec (Parser, ParseError, parse, (<|>), try, eof)

data ReplConfig = ReplConfig
  { file             :: String
  , interactive      :: Bool
  }

data ReplBlock = ReplBlock AST.Block | ReplExpr AST.Expr deriving (Show, Eq)

replBlock :: Parser ReplBlock
replBlock = try (ReplExpr <$> expr <* eof) <|> (ReplBlock . AST.Block <$> block <* eof)

parseLuaRepl :: String -> Either ParseError ReplBlock
parseLuaRepl = parse replBlock ""

runFileFromCommandline :: String -> Context -> IO Context
runFileFromCommandline path ctx = do
    source <- readFile path
    flip execStateT ctx $ do
        let maybeAST = parseLua source

        case maybeAST of
            Right ast -> do
                maybeResult <- state $ runWith ast
                case maybeResult of
                    Right result -> liftIO $ print result
                    Left err -> liftIO . putStrLn $ "Lua error " ++ show err

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

    (flip evalStateT) ctx' $ do
        _ <- state $ \c -> (runIdentity $ runLuaMT loadBaseLibrary c)

        forever $ do
            line <- liftIO $ putStr "> " >> getLine

            let maybeAST = parseLuaRepl line

            case maybeAST of
                Right (ReplBlock b) -> (state $ runWith b) >>= printResult
                Right (ReplExpr e)  -> (state $ evalWith e) >>= printResult
                Left err            -> liftIO . putStrLn $ "Parse error " ++ show err
    where
        -- don't print empty result value (still prints Nil)
        printResult (Right []) = return ()
        printResult (Right result) = liftIO . putStrLn . concatMap prettyPrint $ result
        printResult (Left err) = liftIO . putStrLn $ "Lua error " ++ show err

