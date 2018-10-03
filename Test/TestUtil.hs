module TestUtil where

import qualified Turnip.AST as AST
import Turnip.Parser (parseLua)

successful :: Show a => Either a p -> p
successful (Right x) = x
successful (Left err) = error $ show err

parse :: String -> AST.Block
parse = successful . parseLua
