module Main where

import Test.Hspec

import Parser (parseLua)
import qualified LuaAS as AST
import Eval

import Control.Monad.IO.Class

successful (Right x) = x
successful (Left err) = error $ show err

parse = successful . parseLua
runParse = successful . run . parse

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

        describe "loops" $ do
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

main = hspec spec
