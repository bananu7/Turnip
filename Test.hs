module Test where

import Test.HUnit
import Parser
import LuaAS

successful (Right x) = x
successful _ = error "Computation failed"

test1 = TestCase (assertEqual 
    "for parseLua \"x=5\","
    (successful $ parseLua "x = 5")
    (Block [Assignment [LVar "x"] [Number 5.0]])
    )    

tests = TestList [
    TestLabel "test1" test1
    ]

main = runTestTT tests
