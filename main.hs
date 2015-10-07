module Main where

import Parser( prettyLuaFromFile, loadAST, parseLua )
import Eval

main = do
    input <- readFile "test/fac.lua"
    ast <- loadAST "test/fac.lua"
    run ast