module TestUtil where

import qualified Turnip.AST as AST
import Turnip.Parser (parseLua)
import Turnip.Eval.Types (Value)

successful :: Show a => Either a p -> p
successful (Right x) = x
successful (Left err) = error $ show err

failure :: Either Value [Value] -> [Value]
failure (Right x) = error $ "The call succeeded but failure expected, return: " ++ show x
failure (Left err) = [err]

parse :: String -> AST.Block
parse = successful . parseLua
