module Main where

import Parser( prettyLuaFromFile, loadAST, parseLua )
import Eval

main = do
    --ast <- loadAST "test/fac.lua"
    let maybeAst = parseLua "return 1"
    case maybeAst of
        Right ast -> do
            result <- run ast
            mapM_ print result
        Left err -> print err
