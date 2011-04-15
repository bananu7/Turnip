module Main where

import Parser( prettyLuaFromFile, loadAST )
import Env

main  = loadAST "test/fac.lua"
