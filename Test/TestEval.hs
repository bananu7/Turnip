module Main where

import Test.Hspec

import Parser (parseLua)
import qualified LuaAS as AST
import Eval

import Control.Monad.IO.Class

successful (Right x) = x
successful (Left err) = error $ show err

parse = successful . parseLua

runParse :: String -> [Value]
runParse = successful . run . parse

testFile desc path =
    runIO (readFile $ "Test/lua/" ++ path) >>=
      \fileContents -> it desc $ do
         runParse fileContents `shouldBe` [Boolean True]

spec :: Spec
spec = do
    describe "Eval" $ do
        it "should eval return blocks" $
            runParse "return 1" `shouldBe` [Number 1.0]
        it "should eval operator calls" $ do
            runParse "return 1 + 1" `shouldBe` [Number 2.0]
            runParse "return 3 - 2" `shouldBe` [Number 1.0]
            runParse "return 3 * 8" `shouldBe` [Number 24.0]
        it "should eval if statements" $ do
            runParse "if true then return 5 end" `shouldBe` [Number 5.0]
            runParse "if false then return 0 end; return 1" `shouldBe` [Number 1.0]
            runParse "if false then else return 6 end" `shouldBe` [Number 6.0]
            runParse "if false then elseif true then return 3 end" `shouldBe` [Number 3.0]
            runParse "if false then elseif false then else return 2 end" `shouldBe` [Number 2.0]

        it "should eval functions" $ do
            runParse "function f() end; return f()" `shouldBe` [Nil]
            runParse "function f() return 5 end; return f()" `shouldBe` [Number 5.0]
            runParse "function f(x) return x end; return f(6)" `shouldBe` [Number 6.0]

        it "should eval more complex functions" $ do
            runParse "function add(a,b) return a+b end; return add(3,4)" `shouldBe` [Number 7.0]
            runParse "function out(x) function inner() return x end; return inner(); end; return out(3)" `shouldBe` [Number 3.0]
            runParse "function out(x) function inner(y) return x+y end; return inner(5); end; return out(3)" `shouldBe` [Number 8.0]

        it "should properly scope locals" $ do
            runParse "x = 1; function f() local x = 2; return x end; return f()" `shouldBe` [Number 2.0]
            runParse "function f() local x = 2; local function g() return x end; return g; end; return f()()" `shouldBe` [Number 2.0]
            runParse (unlines [
                 "function f()"
                ,"  local x = 2;"
                ,"  local function g()"
                ,"    x = 3"
                ,"  end"
                ,"  local function h()"
                ,"    return x"
                ,"  end"
                ,"  return g, h"
                ,"end"
                ,"g,h = f();"
                ,"g();"
                ,"return h();"])
                 `shouldBe` [Number 3.0]

        it "should properly prefer local assignment" $ do
            runParse (unlines [
                 "x = 1"
                ,"function f()"
                ,"  local x = 2"
                ,"end"
                ,"f()"
                ,"return x"
                ]) `shouldBe` [Number 1.0]

        it "should properly declare local missing multiple assignments" $ do
            runParse (unlines [
                 "x, y = 1,2"
                ,"function f()"
                ,"  local x,y = 3" -- y doesn't receive a value
                ,"  y = 5"         -- but should still be declared as a local
                ,"end"
                ,"return y"
                ]) `shouldBe` [Number 2.0]

        describe "assignments" $ do
            it "should handle trivial assignments" $ do
                runParse "x = 1; return x" `shouldBe` [Number 1.0]
                runParse "x = 1; x = 2; return x" `shouldBe` [Number 2.0]
            it "should handle multiple assignments" $ do
                runParse "x, y = 1, 2; return x, y" `shouldBe` [Number 1.0, Number 2.0]
                runParse "x, y = 1; return x, y" `shouldBe` [Number 1.0, Nil]
                runParse "x, y = 1, 2, 3; return x, y" `shouldBe` [Number 1.0, Number 2.0]

        describe "tables" $ do
            it "should handle table assignments" $ do
                runParse "t = {}; t[1] = 4; return t[1]" `shouldBe` [Number 4.0]

        describe "while loop" $ do
            it "should properly skip a loop with a false clause" $ do
                runParse "while false do return 3 end return 1" `shouldBe` [Number 1.0]

            it "should properly break out of a trivial while-loop" $ do
                runParse (unlines [
                     "while true do"
                    ,"  break"
                    ,"end"
                    ,"return 1"
                    ]) `shouldBe` [Number 1.0]

            it "should properly return out of a trivial while-loop" $ do
                runParse (unlines [
                     "while true do"
                    ,"  return 2"
                    ,"end"
                    ,"return 1"
                    ]) `shouldBe` [Number 2.0]

            it "should properly handle a simple while-loop" $ do
                runParse (unlines [
                     "x = 1"
                    ,"while x < 5 do"
                    ,"  x = x + 1"
                    ,"end"
                    ,"return x"
                    ]) `shouldBe` [Number 5.0]

            it "should correctly handle breaking out of a nested loop" $ do
                runParse (unlines [
                     "x = 1"
                    ,"c = 1"
                    ,"while c < 3 do"
                    ,"  c = c+1"         -- this is just a counter
                    ,"  local y = 1"
                    ,"  while y < 2 do"  -- this could theoretically
                    ,"    x = x * 2"     -- multiply x by 4
                    ,"    break"         -- however, it only does multiplication by 2
                    ,"  end"             -- thus the value of x becomes
                    ,"end"               -- 1 * 2 * 2 = 4
                    ,"return x"
                    ]) `shouldBe` [Number 4.0]

        describe "for loop" $ do
            it "should correctly handle a trivial for loop" $ do
                runParse (unlines [
                     "local t = function()"
                    ,"  local f = function() return nil end" -- iterator fn
                    ,"  local s = false" -- state invariant
                    ,"  local v = false" -- iteration variable returned by f
                    ,"  return f, s, v"
                    ,"end"
                    ,"for k,v in t() do"
                    ,"end"
                    ,"return true"
                    ]) `shouldBe` [Boolean True]

            testFile "should correctly handle a synthetic iterator" "for-loop-basic.lua"

            it "should properly scope iteration-for-loop variables" $ do
                runParse (unlines[
                     "function f()"
                    ,"  local x = 5"
                    ,"  for x in (function() return nil end) do"
                    ,"  end"
                    ,"  return x"
                    ,"end"
                    ,"return f()"
                    ]) `shouldBe` [Number 5.0]

            testFile "should correctly handle a simple numeric loop" "for-loop-numeric.lua"
            testFile "should correctly handle a reverse numeric loop" "for-loop-numeric-reverse.lua"

            it "should correctly handle for loops that shouldn't run even once" $ do
                runParse "for x = 2,1 do return false end return true" `shouldBe` [Boolean True]
                runParse "for x = 1,2,-1 do return false end return true" `shouldBe` [Boolean True]


main = hspec spec
