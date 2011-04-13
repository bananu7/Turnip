module Main where

import Parser( prettyLuaFromFile )

main  = prettyLuaFromFile "test/fac.lua"
