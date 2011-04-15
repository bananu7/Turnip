module Main where

import Parser( prettyLuaFromFile, loadAST )
import Env

main  
    = do{ ast <- loadAST "test/fac.lua"
        ; let stmts = statements ast
        ; print ast
        }
