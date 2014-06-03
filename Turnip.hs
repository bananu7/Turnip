module Main where

import Parser( prettyLuaFromFile, loadAST, parseLua )
import Env

main  
    = do{ input <- readFile "test/fac.lua"
        ; ast <- loadAST "test/fac.lua"
        ; putStr input
--        ; let stmts = statements ast
        ; print ast
        }
