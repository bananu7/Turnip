module Main where

import Parser( prettyLuaFromFile, loadAST, parseLua )
import Eval

main = do
    --ast <- loadAST "test/fac.lua"
    let maybeAst = parseLua "return 1"
    case maybeAst of
        Right ast -> run ast
        Left err -> print err
