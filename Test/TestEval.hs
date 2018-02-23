module Main where

import Test.Hspec

import Turnip.Parser (parseLua)
import qualified Turnip.AST as AST
import Turnip.Eval

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

        describe "should eval operator calls" $ do
            it "+" $ runParse "return 1 + 1" `shouldBe` [Number 2.0]
            it "-" $ runParse "return 3 - 2" `shouldBe` [Number 1.0]
            it "*" $ runParse "return 3 * 8" `shouldBe` [Number 24.0]
            it "/" $ runParse "return 9 / 3" `shouldBe` [Number 3.0]

        describe "logical operators" $ do
            it "not" $ runParse "return not nil, not true, not false, not 5, not \"\"" `shouldBe`
                (map Boolean [True, False, True, False, False])
            it "or" $ runParse "return true and true, true and false, false and true, false and false" `shouldBe`
                (map Boolean [True, False, False, False])
            it "and" $ runParse "return true or true, true or false, false or true, false or false" `shouldBe`
                (map Boolean [True, True, True, False])

        describe "should eval 'if' statements" $ do
            it "trivial always-true" $
                runParse "if true then return 5 end" `shouldBe` [Number 5.0]
            it "trivial always false (skip)" $
                runParse "if false then return 0 end; return 1" `shouldBe` [Number 1.0]
            it "false with else" $
                runParse "if false then else return 6 end" `shouldBe` [Number 6.0]
            it "false with true elseif" $
                runParse "if false then elseif true then return 3 end" `shouldBe` [Number 3.0]
            it "false, with false elseif and else" $
                runParse "if false then elseif false then else return 2 end" `shouldBe` [Number 2.0]

        describe "functions" $ do
            describe "should eval functions" $ do
                it "trivial empty" $
                    runParse "function f() end; return f()" `shouldBe` [Nil]
                it "simple return" $
                    runParse "function f() return 5 end; return f()" `shouldBe` [Number 5.0]
                it "return of a passed argument" $
                    runParse "function f(x) return x end; return f(6)" `shouldBe` [Number 6.0]

            describe "should eval more complex functions" $ do
                it "two parameters" $
                    runParse "function add(a,b) return a+b end; return add(3,4)" `shouldBe` [Number 7.0]
                it "with an inner closure returning the outer parameter" $
                    runParse "function out(x) function inner() return x end; return inner(); end; return out(3)" `shouldBe` [Number 3.0]
                it "with an inner closure taking a parameter and combining with outer one" $
                    runParse "function out(x) function inner(y) return x+y end; return inner(5); end; return out(3)" `shouldBe` [Number 8.0]

            it "should properly eval recursive functions" $ do
                runParse "function f(x) if x == 0 then return x else return f(x-1) end end; return f(5)" `shouldBe` [Number 0.0]

            describe "should properly scope locals" $ do
                it "declaring and returning a local variable that shadows a global" $
                    runParse "x = 3; function f() local x = 1; return x end; return f()" `shouldBe` [Number 1.0]

                it "returning a closure" $
                    runParse (unlines [
                         "function f()"
                        ,"  local x = 2;"
                        ,"  local function g()"
                        ,"    return x"
                        ,"  end"
                        ,"  return g"
                        ,"end"
                        ,"return f()()"])
                         `shouldBe` [Number 2.0]

                it "returning two interacting closures" $
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

            it "should properly fill in nils for missing argument values" $ do
                runParse (unlines [
                     "x = 5"
                    ,"function f(x)"
                    ,"  return x"
                    ,"end"
                    ,"return f()"
                    ]) `shouldBe` [Nil]

            it "should properly forward multiple returns" $ do
                runParse (unlines [
                     "function f()"
                    ,"  return 1,2,3"
                    ,"end"
                    ,"return f()"
                    ]) `shouldBe` [Number 1.0, Number 2.0, Number 3.0]

            describe "vararg functions" $ do
                describe "arg" $ do
                    it "no arguments to a vararg functions should result in an empty table" $
                        runParse "function f(...) return arg[1] end; return f()" `shouldBe` [Nil]
                    it "trying to access 'arg' in a non-vararg function should produce nil" $
                        runParse "function f() return arg end; return f()" `shouldBe` [Nil]
                    it "just varargs" $ do
                        runParse (unlines [
                             "function f(...)"
                            ,"  return arg[1], arg[2], arg[3]"
                            ,"end"
                            ,"return f(1,2)"
                            ]) `shouldBe` [Number 1.0, Number 2.0, Nil]
                    it "varargs should play nice with normal parameters" $
                        runParse (unlines [
                             "function f(x, ...)"
                            ,"  return x, arg[1], arg[2]"
                            ,"end"
                            ,"a,b,c = f(2,3)"
                            ,"return a,b,c"
                            ]) `shouldBe` [Number 2.0, Number 3.0, Nil]
                describe "ellipsis" $ do
                    it "should eval ellipsis to a vararg value pack" $
                        runParse (unlines [
                             "function f(...)"
                            ,"  return(...)"
                            ,"end"
                            ,"return f(1,2)"
                            ]) `shouldBe` [Number 1.0, Number 2.0]

                    it "should properly scope nested ellipsis" $
                        runParse (unlines [
                             "function f(...)"
                            ,"  function g(...)"
                            ,"     return(...)"
                            ,"  end"
                            ,"  return g(...), g(3,4)" -- returns first of each pack
                            ,"end"
                            ,"return f(1,2)"
                            ]) `shouldBe` [Number 1.0, Number 3.0]

                    it "should properly eval ellipsis together with normal parameters" $
                        runParse (unlines [
                             "function f(x, y, ...)"
                            ,"  return ..."
                            ,"end"
                            ,"a,b,c,d = f(1,2,3,4)"
                            ,"return a,b,c,d"
                            ]) `shouldBe` [Number 3.0, Number 4.0, Nil, Nil]

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
            it "should handle consecutive array constructors" $ do
                runParse "t = { 4, 5, 6 }; return t[0], t[1], t[2], t[3]" 
                    `shouldBe` [Nil, Number 4.0, Number 5.0, Number 6.0]
            it "should handle mixed array constructors" $ do
                runParse "t = { 1, x = 2 }; return t[1], t[\"x\"]"
                    `shouldBe` [Number 1.0, Number 2.0]
                runParse "t = { 1, x = 2, 3 }; return t[1], t[\"x\"], t[2]"
                    `shouldBe` [Number 1.0, Number 2.0, Number 3.0]
            it "should handle table access syntax sugar" $ do
                runParse "t = { x = 1 }; return t.x" `shouldBe` [Number 1.0]
                runParse "t = {}; t.x = 2; return t[\"x\"]" `shouldBe` [Number 2.0]
            it "should handle method call (:) syntax sugar" $ do
                runParse "t = { f = function() return 1 end }; return t:f()"
                    `shouldBe` [Number 1.0]
            it "should properly pass self to method calls" $ do
                runParse "t = { x = 5, f = function(self) return self.x end }; return t:f()"
                    `shouldBe` [Number 5.0]

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

        describe "repeat..until loop" $ do
            it "should correctly handle a one-run loop" $ do
                runParse (unlines[
                     "x = 2"
                    ,"repeat"
                    ,"  x = x + 1"
                    ,"until true"
                    ,"return x"
                    ]) `shouldBe` [Number 3.0]

            it "should correctly handle a multiple-run loop" $ do
                runParse (unlines[
                     "x = 2"
                    ,"r = 3"
                    ,"repeat"
                    ,"  x = x + 1"
                    ,"  r = r - 1"
                    ,"until r == 0"
                    ,"return x, r"
                    ]) `shouldBe` [Number 5.0, Number 0.0]


        describe "do..end" $ do
            it "should properly scope do-blocks" $ do
                runParse (unlines[
                     "x = 5"
                    ,"y = 3"
                    ,"do"
                    ,"  local x = 4"
                    ,"  y = x"         -- check if x will resolve to local
                    ,"end"
                    ,"return x, y"
                    ]) `shouldBe` [Number 5.0, Number 4.0]

        describe "pcall" $ do
            it "should properly contain simple errors" $ do
                runParse (unlines[
                     "function f()"
                    ,"  error(\"test\")"
                    ,"end"
                    ,"pcall(f)"
                    ,"return 1"
                    ]) `shouldBe` [Number 1.0]

            it "should properly forward on success" $ do
                runParse (unlines[
                     "function f()"
                    ,"  return 1,2,3"
                    ,"end"
                    ,"return pcall(f)"
                    ]) `shouldBe` [Boolean True, Number 1.0, Number 2.0, Number 3.0]

            it "should return false and the error in case of errors" $ do
                runParse ("return pcall(function() error(\"test\") end)")
                    `shouldBe` [Boolean False, Str "test"]

main = hspec spec
