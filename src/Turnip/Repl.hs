{-# LANGUAGE FlexibleContexts #-}

module Turnip.Repl (repl, ReplConfig(..)) where

import qualified Turnip.Parser as Parser
import Turnip.Eval
import Turnip.Eval.Lib (loadBaseLibrary)
import Turnip.Eval.Types (LuaMT)
import qualified Turnip.AST as AST
import Turnip.PrettyPrint
import Text.ParserCombinators.Parsec (Parser, ParseError, parse, (<|>), try, eof)

import Paths_Turnip (version)
import Data.Version (showVersion)

import Control.Monad.State
import System.IO
import Control.Monad.Except (catchError)
import Data.List (intercalate)

data ReplConfig = ReplConfig
  { file             :: String
  , interactive      :: Bool
  }

data ReplBlock = ReplBlock AST.Block | ReplExpr AST.Expr deriving (Show, Eq)

replBlock :: Parser ReplBlock
replBlock = try (ReplExpr <$> Parser.expr <* eof) <|> (ReplBlock . AST.Block <$> Parser.block <* eof)

parseLuaRepl :: String -> Either ParseError ReplBlock
parseLuaRepl = parse replBlock ""

runFileFromCommandline :: (MonadIO m) => String -> LuaMT m ()
runFileFromCommandline path = do
    source <- liftIO $ readFile path

    let maybeAST = Parser.parseLua source
    case maybeAST of
        Right block -> (execBlockResult block >>= printResult) `catchError` printError
        Left err -> printParseError err

disableBuffering :: IO ()
disableBuffering = hSetBuffering stdout NoBuffering

repl :: ReplConfig -> IO ()
repl cfg = do
    disableBuffering 
    putStrLn $ "Turnip REPL v" ++ showVersion version ++ "\n"

    _ <- flip runLuaMT defaultCtx $ do
        loadBaseLibrary

        let filePath = file cfg
        when (filePath /= "") $ runFileFromCommandline filePath

        forever $ do
            line <- liftIO $ putStr "> " >> getLine

            let maybeAST = parseLuaRepl line
            case maybeAST of
                Right (ReplBlock b) -> (execBlockResult b >>= printResult) `catchError` printError
                Right (ReplExpr e)  -> (eval e >>= printResult) `catchError` printError
                Left err            -> printParseError err
    return ()

printResult :: MonadIO m => [Value] -> LuaMT m ()
printResult [] = return () -- don't print empty result value (still prints Nil)
printResult result = liftIO . putStrLn . intercalate ", " . map prettyPrint $ result

printError :: MonadIO m => Value -> LuaMT m ()
printError err = liftIO . putStrLn $ "Lua error " ++ show err

printParseError :: MonadIO m => ParseError -> LuaMT m ()
printParseError err = liftIO . putStrLn $ "Parse error " ++ show err

