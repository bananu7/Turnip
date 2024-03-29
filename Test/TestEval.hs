module Main where

import Test.Hspec

import Turnip.Eval
import TestUtil

import Data.List (isPrefixOf)

runParse :: String -> [Value]
runParse = successful . runWithDefault . parse

runParseFail :: String -> [Value]
runParseFail = failure . runWithDefault . parse

testFile :: String -> String -> Spec
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
            describe "basic arithmetic" $ do
                it "+" $ runParse "return 1 + 1" `shouldBe` [Number 2.0]
                it "-" $ runParse "return 3 - 2" `shouldBe` [Number 1.0]
                it "*" $ runParse "return 3 * 8" `shouldBe` [Number 24.0]
                it "/" $ runParse "return 9 / 3" `shouldBe` [Number 3.0]
                it "%" $ runParse "return 4 % 3" `shouldBe` [Number 1.0]
                it "^" $ runParse "return 2 ^ 4" `shouldBe` [Number 16.0]
            describe "comparisons" $ do
                it ">" $ runParse "return 1 > 2, 2 > 1, 1 > 1" `shouldBe` (map Boolean [False, True, False])
                it "<" $ runParse "return 1 < 2, 2 < 1, 1 < 1" `shouldBe` (map Boolean [True, False, False])
                it ">=" $ runParse "return 1 >= 2, 2 >= 1, 1 >= 1" `shouldBe` (map Boolean [False, True, True])
                it "<=" $ runParse "return 1 <= 2, 2 <= 1, 1 <= 1" `shouldBe` (map Boolean [True, False, True])
            it "concat (..)" $ runParse "return \"abc\" .. \"def\"" `shouldBe` [Str "abcdef"]

        describe "length operator" $ do
            describe "tables" $ do
                it "empty table literal" $ runParse "return #{}" `shouldBe` [Number 0.0]
                it "simple table literal" $ runParse "return #{1,2,3}" `shouldBe` [Number 3.0]
                it "table in a variable" $ runParse "x = {1,2}; return #x" `shouldBe` [Number 2.0]
                it "table with mixed keys" $ runParse "t = {1, a=2, 3}; return #t" `shouldBe` [Number 2.0]
                it "table with holes" $ runParse "t = {[1]=1, [3]=2, [4]=3}; return #t" `shouldSatisfy` (\[Number i] -> i `elem` [1.0, 4.0])
            describe "strings" $ do
                it "empty string literal" $ runParse "return #\"\"" `shouldBe` [Number 0.0]
                it "simple string literal" $ runParse "return #\"abc\"" `shouldBe` [Number 3.0]

        describe "equality" $ do
            it "numbers" $ runParse "return 1 == 1, 1 == -1, 1 == 2, 2 == 1" 
                `shouldBe` (map Boolean [True, False, False, False])
            it "strings" $ runParse "return \"a\" == \"a\",\"a\" == \"\", \"a\" == \"ab\", \"b\" == \"a\", \"\" == false"
                `shouldBe` (map Boolean [True, False, False, False, False])
            it "booleans" $ runParse "return true == true, false == false, true == false, false == true"
                `shouldBe` (map Boolean [True, True, False, False])
            it "nil" $ runParse "return nil == nil, nil == 1, nil == \"a\", nil == {}"
                `shouldBe` (map Boolean [True, False, False, False])
            it "tables" $ runParse "return {} == {}, {\"a\"} == \"a\", {42} == 42, {} == false"
                `shouldBe` (map Boolean [False, False, False, False])
            it "not-equality" $ runParse "return {} ~= {}, {42} ~= 42, nil ~= nil, false ~= false"
                `shouldBe` (map Boolean [True, True, False, False])


        describe "logical operators" $ do
            describe "basics" $ do
                it "not" $ runParse "return not nil, not true, not false, not 5, not \"\"" `shouldBe`
                    (map Boolean [True, False, True, False, False])
                it "or" $ runParse "return true and true, true and false, false and true, false and false" `shouldBe`
                    (map Boolean [True, False, False, False])
                it "and" $ runParse "return true or true, true or false, false or true, false or false" `shouldBe`
                    (map Boolean [True, True, True, False])
            describe "passthrough" $ do
                describe "or" $ do
                    it "both true" $ do
                        runParse "return 1 or 2" `shouldBe` [Number 1.0]
                        runParse "return true or 5" `shouldBe` [Boolean True]
                    it "first false" $ do
                        runParse "return false or 3" `shouldBe` [Number 3.0]
                        runParse "return nil or 4" `shouldBe` [Number 4.0]
                        runParse "return false or true" `shouldBe` [Boolean True]
                    it "NaN coerces to false" $
                        runParse "return 0/0 or 42" `shouldBe` [Number 42.0]
                    it "both coerced false" $ do
                        runParse "return false or nil" `shouldBe` [Nil]
                        runParse "return nil or nil" `shouldBe` [Nil]
                    it "doesn't eval 2nd if first true" $
                        runParse "x = 1; function f() x = 2 end; return true or f(), x"
                             `shouldBe` [Boolean True, Number 1.0]

                describe "and" $ do
                    it "both true" $ runParse "return 1 and 2" `shouldBe` [Number 2.0]
                    it "first false" $ runParse "return false and 3" `shouldBe` [Boolean False]
                    it "doesn't eval 2nd if first false" $
                        runParse "x = 1; function f() x = 2 end; return false and f(), x"
                            `shouldBe` [Boolean False, Number 1.0]

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
                    runParse "function f() end; return f()" `shouldBe` []
                it "empty return" $
                    runParse "function f() return end; return f()" `shouldBe` []
                it "return nil" $ -- see special case #70
                    runParse "function f() return nil end; return f()" `shouldBe` [Nil]
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

            describe "pack spills" $ do
                it "should properly spill a pack into arguments" $ 
                    runParse (unlines [
                         "function f() return 1,2,3 end"
                        ,"function g(a,b,c) return a,b,c end"
                        ,"return g(f())"
                        ]) `shouldBe` [Number 1.0, Number 2.0, Number 3.0]
                it "should properly handle multiple packs" $
                    -- in case there are multiple packs, only the last one spills
                    runParse (unlines [
                         "function f() return 1,2 end"
                        ,"function g() return 3,4 end"
                        ,"function h(a,b,c,d) return a,b,c,d end"
                        ,"return h(f(), g())"
                        ]) `shouldBe` [Number 1.0, Number 3.0, Number 4.0, Nil]
                it "should also spill in member calls" $
                    runParse (unlines [
                         "t = { f = function(self, a,b) return a,b end }"
                        ,"function g() return 1,2 end"
                        ,"return t:f(g())" 
                        ]) `shouldBe` [Number 1.0, Number 2.0]

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
            it "nil shouldn't be a valid table key" $ do
                runParseFail "t = {}; t[nil] = 42" `shouldBe` [Str "Table index is nil"]
                runParseFail "t = { [nil] = 42 }" `shouldBe` [Str "Table index is nil"]

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
            testFile "should correctly handle a generic loop with kv iter" "for-loop-kv.lua"

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

        describe "standard library" $ do
            describe "assert" $ do
                it "not hit if true" $
                    runParse "assert(true); return 1" `shouldBe` [Number 1.0]
                it "no description" $
                    runParseFail "assert(false)" `shouldBe` [Str "assertion failed!"]
                it "error value" $
                    runParseFail "assert(false, 42)" `shouldBe` [Number 42.0]

            describe "error" $ do
                it "stops the execution and errs out" $
                    runParseFail "error(42); return 43" `shouldBe` [Number 42.0]

            describe "next" $ do
                it "properly returns nil on an empty table" $ do
                    runParse "return next({})" `shouldBe` [Nil]
                    runParse "return next({}, nil)" `shouldBe` [Nil]
                it "properly returns first result in a numeric table" $ do
                    runParse "return next({4,5,6})" `shouldBe` [Number 1, Number 4]
                    runParse "return next({4,5,6}, nil)" `shouldBe` [Number 1, Number 4]
                it "properly returns first result in a keyed table" $ do
                    runParse "return next({x = 1, y = 2, z = 3})" `shouldBe` [Str "x", Number 1]
                    runParse "return next({x = 1, y = 2, z = 3}, nil)" `shouldBe` [Str "x", Number 1]
                it "properly iterates until the end in a numeric table" $
                    runParse (unlines[
                         "t={4,5,6}"
                        ,"a,b = next(t)"    -- 1,4
                        ,"c,d = next(t,a)"  -- 2,5
                        ,"e,f = next(t,c)"  -- 3,6
                        ,"g = next(t, e)" -- nil
                        ,"return a,b,c,d,e,f,g"
                        ]) `shouldBe` (map Number [1,4,2,5,3,6]) ++ [Nil]

            describe "pairs" $ do
                -- one could add a test that verifies whether pairs returns next etc.
                -- but I think that a functional test is preferrable here.
                it "allows iteration" $
                    runParse (unlines[
                         "t={4, 5, x=6, 7}"
                        ,"u={}"
                        ,"for k,v in pairs(t) do u[k]=v end"
                        ,"return u[1], u[2], u[3], u.x"
                        ]) `shouldBe` [Number 4, Number 5, Number 7, Number 6]

            describe "ipairs" $ do
                it "allows iteration" $
                    runParse (unlines[
                         "t={4, 5, x=6, 7}"
                        ,"u={}"
                        ,"for k,v in ipairs(t) do u[k] = v end"
                        ,"return u[1], u[2], u[3], u.x"
                        ]) `shouldBe` [Number 4, Number 5, Number 7, Nil]

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

                it "should work with non-string errors" $ do
                    runParse ("return pcall(function() error() end)") `shouldBe` [Boolean False, Nil]
                    runParse ("return pcall(function() error(42) end)") `shouldBe` [Boolean False, Number 42]

                it "should properly rely args to the function" $
                    runParse (unlines[
                         "x = 1"
                        ,"pcall(function(nx) x = nx end, 2)"
                        ,"return x"
                        ]) `shouldBe` [Number 2.0]

            describe "tostring" $ do
                it "booleans" $ do
                    runParse ("return tostring(true)") `shouldBe` [Str "true"]
                    runParse ("return tostring(false)") `shouldBe` [Str "false"]

                it "numbers" $ do
                    runParse ("return tostring(42)") `shouldBe` [Str "42"]
                    runParse ("return tostring(3.14)") `shouldBe` [Str "3.14"]

                it "strings" $ do
                    runParse ("return tostring(\"\")") `shouldBe` [Str ""]
                    runParse ("return tostring(\"xyz\")") `shouldBe` [Str "xyz"]

                it "nil" $
                    runParse ("return tostring(nil)") `shouldBe` [Str "nil"]

            describe "select" $ do
                it "should return the length of the pack with '#'" $ do
                    runParse "return select(\"#\")" `shouldBe` [Number 0.0]
                    runParse "return select(\"#\", 1)" `shouldBe` [Number 1.0]
                    runParse "return select(\"#\", nil, \"b\")" `shouldBe` [Number 2.0]
                    runParse "return select(\"#\", 1, nil, 2, nil)" `shouldBe` [Number 4.0]
                it "should return appropriate cutoff pack" $ do
                    runParse "return select(1, 1)" `shouldBe` [Number 1.0]
                    runParse "return select(1, 3, 4)" `shouldBe` [Number 3.0, Number 4.0]
                    runParse "return select(2, 3, 4)" `shouldBe` [Number 4.0]
                    runParse "return select(3, 3, 4)" `shouldBe` []
                    runParse "return select(4, 3, 4)" `shouldBe` []
                it "should properly work with function returns" $ do
                    runParse (unlines [
                         "function f() return 42,43,44 end"
                        ,"return select(2, f())"
                        ]) `shouldBe` [Number 43.0, Number 44.0]

            describe "rawget" $ do
                it "without a metatable/__index" $
                    runParse (unlines[
                         "t = { x = 42 }"
                        ,"return rawget(t, \"x\")"
                        ]) `shouldBe` [Number 42.0]
                it "with __index, missing key" $
                    runParse (unlines[
                         "t = { }"
                        ,"setmetatable(t, { __index = function(t,i) return 43 end })"
                        ,"return rawget(t, \"x\"), t.x"
                        ]) `shouldBe` [Nil, Number 43.0]
                it "with __index, existing key" $
                    runParse (unlines[
                         "t = { x = 5 }"
                        ,"setmetatable(t, { __index = function(t,i) return 43 end })"
                        ,"return rawget(t, \"x\"), t.x"
                        ]) `shouldBe` [Number 5.0, Number 5.0]

            describe "rawlen" $ do
                it "strings" $ 
                    runParse (unlines[
                        "return rawlen(\"xyz\")"
                        ]) `shouldBe` [Number 3.0]

                it "table w/o __len" $ do
                    runParse (unlines[
                         "t = {1,2,3,4}"
                        ,"return rawlen(t), #t"
                        ]) `shouldBe` [Number 4.0, Number 4.0]

                it "table with __len" $ do
                    runParse (unlines[
                         "t = {1,2,3,4}"
                        ,"mt = { __len = function() return 42 end }"
                        ,"setmetatable(t, mt)"
                        ,"return rawlen(t), #t"
                        ]) `shouldBe` [Number 4.0, Number 42.0]

            describe "rawequal" $ do
                it "nil" $
                    runParse ("return rawequal(nil,nil)") `shouldBe` [Boolean False]
                it "numbers" $ do
                    runParse ("return rawequal(1,1)") `shouldBe` [Boolean True]
                    runParse ("return rawequal(1,2)") `shouldBe` [Boolean False]
                it "strings" $ do
                    runParse ("return rawequal(\"\", \"\")") `shouldBe` [Boolean True]
                    runParse ("return rawequal(\"\", \"x\")") `shouldBe` [Boolean False]
                describe "tables" $ do
                    it "without metatable" $
                        runParse ("return rawequal({}, {})") `shouldBe` [Boolean False]
                    it "with __eq" $
                        runParse (unlines[
                             "t,u = {}, {}"
                            ,"mt = { __eq = function() return true end }"
                            ,"setmetatable(t, mt)"
                            ,"setmetatable(u, mt)"
                            ,"return t == u, rawequal(t, u)"
                            ]) `shouldBe` [Boolean True, Boolean False]

            describe "tonumber" $ do
                it "number passtrough" $ do
                    runParse ("return tonumber(0)") `shouldBe` [Number 0]
                    runParse ("return tonumber(-1)") `shouldBe` [Number (-1)]
                    runParse ("return tonumber(3.14)") `shouldBe` [Number 3.14]
                    -- runParse ("return tonumber(1.)") `shouldBe` [Number 1.0] -- TODO - fix Parser!
                    -- runParse ("return tonumber(.123)") `shouldBe` [Number 0.123] -- TODO - fix Parser!

                describe "string parse" $ do
                    it "0" $ runParse ("return tonumber(\"0\")") `shouldBe` [Number 0]
                    it "-1" $ runParse ("return tonumber(\"-1\")") `shouldBe` [Number (-1)]
                    it "+42" $ runParse ("return tonumber(\"+42\")") `shouldBe` [Number 42]
                    it "3.14" $ runParse ("return tonumber(\"3.14\")") `shouldBe` [Number 3.14]
                    it "1." $ runParse ("return tonumber(\"1.\")") `shouldBe` [Number 1.0]
                    it ".123" $ runParse ("return tonumber(\".123\")") `shouldBe` [Number 0.123]

                describe "whitespace surround" $ do
                    it " -2" $ runParse ("return tonumber(\" -2\")") `shouldBe` [Number (-2)]
                    it "-3 " $ runParse ("return tonumber(\"-3 \")") `shouldBe` [Number (-3)]
                    it " 4 " $ runParse ("return tonumber(\" 4 \")") `shouldBe` [Number 4]

                describe "different base" $ do
                    it "binary" $ do
                        runParse ("return tonumber(\"100\", 2)") `shouldBe` [Number 4]
                        runParse ("return tonumber(\"101\", 2)") `shouldBe` [Number 5]
                        runParse ("return tonumber(\"110\", 2)") `shouldBe` [Number 6]
                        runParse ("return tonumber(\"0111\", 2)") `shouldBe` [Number 7]
                        runParse ("return tonumber(\"01011001\", 2)") `shouldBe` [Number 89] -- ;)
                    it "negative binary" $
                        runParse ("return tonumber(\"-1\", 2)") `shouldBe` [Number (-1)]
                    it "other bases" $ do
                        runParse ("return tonumber(\"ff\", 16)") `shouldBe` [Number 255]
                        runParse ("return tonumber(\"FF\", 16)") `shouldBe` [Number 255]
                        runParse ("return tonumber(\"+ff\", 16)") `shouldBe` [Number 255]
                        runParse ("return tonumber(\"-ff\", 16)") `shouldBe` [Number (-255)]
                        runParse ("return tonumber(\"-FF\", 16)") `shouldBe` [Number (-255)]
                        runParse ("return tonumber(\"10\", 36)") `shouldBe` [Number 36]

                describe "failed conversions" $ do
                    it "nil" $ runParse ("return tonumber(nil)") `shouldBe` [Nil]
                    it "{}" $ runParse ("return tonumber({})") `shouldBe` [Nil]
                    it "false" $ runParse ("return tonumber(false)") `shouldBe` [Nil]
                    it "true" $ runParse ("return tonumber(true)") `shouldBe` [Nil]
                    it "function()" $ runParse ("return tonumber(function()end)") `shouldBe` [Nil]
                    it "\"f\"" $ runParse ("return tonumber(\"f\")") `shouldBe` [Nil]
                    it "\"3-10\"" $ runParse ("return tonumber(\"3-10\")") `shouldBe` [Nil]

            describe "load" $ do
                it "loads trivial code" $
                    runParse ("load('x = 1')(); return x") `shouldBe` [Number 1]
                it "loads from a function" $
                    runParse (unlines[
                         "function f() if not x then x = true; return \"y = 2\" else return nil end end"
                        ,"g = load(f)"
                        ,"g()"
                        ,"return y"
                        ])`shouldBe` [Number 2]
                it "properly fails on wrong code" $
                    runParseFail ("load('this is not lua code')") `shouldSatisfy` (\[Str err] -> "Parse error" `isPrefixOf` err)


        describe "_G" $ do
            it "should expose _G table" $
                runParse "x = 5; return _G.x" `shouldBe` [Number 5.0]

        it "_VERSION" $ do
            runParse "return type(_VERSION)" `shouldBe` [Str "string"]

        describe "metatables" $ do
            it "should allow setting and getting the metatable" $
                runParse (unlines [
                     "a = { x = 7 }"
                    ,"b = { x = 8 }"
                    ,"setmetatable(a, b)"
                    ,"return getmetatable(a).x"
                    ]) `shouldBe` [Number 8.0]

            it "default tables should have no metatable" $
                runParse "return getmetatable({})" `shouldBe` [Nil]

            describe "metatable operators" $ do
                it "should allow setting the __unm metafunction" $
                    runParse (unlines [
                         "t = { x = 5 }"
                        ,"setmetatable(t, { __unm = function(t) return -t.x end })"
                        ,"return -t"
                        ]) `shouldBe` [Number (-5.0)]

                it "should allow setting the __add metafunction" $
                    runParse (unlines [
                         "t = { x = 42 }"
                        ,"setmetatable(t, { __add = function(a,b) return a + b.x end })"
                        ,"return 123 + t"
                        ]) `shouldBe` [Number 165.0]

                it "should allow setting the __mult metafunction" $
                    runParse (unlines [
                         "t = { x = 42 }"
                        ,"setmetatable(t, { __mult = function(a,b) return a * b.x end })"
                        ,"return 2 * t"
                        ]) `shouldBe` [Number 84.0]

                it "should allow setting the __sub metafunction" $
                    runParse (unlines [
                         "t = { x = 10 }"
                        ,"setmetatable(t, { __sub = function(a,b) return a - b.x end })"
                        ,"return 42 - t"
                        ]) `shouldBe` [Number 32.0]

                it "should allow setting the __div metafunction" $
                    runParse (unlines [
                         "t = { x = 42 }"
                        ,"setmetatable(t, { __div = function(a,b) return a.x / b end })"
                        ,"return t / 2"
                        ]) `shouldBe` [Number 21.0]

                it "should allow setting the __concat metafunction" $
                    runParse (unlines [
                         "t = { x = \"456\" }"
                        ,"setmetatable(t, { __concat = function(a,b) return a .. b.x end })"
                        ,"return \"123\" .. t"
                        ]) `shouldBe` [Str "123456"]

                it "should allow setting __len metafunction" $
                    runParse (unlines [
                         "t = { x = 42 }"
                        ,"setmetatable(t, { __len = function(a) return a.x end })"
                        ,"return #t"
                        ]) `shouldBe` [Number 42.0]

            describe "metatable comparators" $ do
                it "should allow setting the __lt metafunction" $
                    runParse (unlines [
                         "t = { x = 4 }"
                        ,"setmetatable(t, { __lt = function(a,b) return a.x < b end })"
                        ,"return t < 3, t < 4, t < 5"
                    ]) `shouldBe` [Boolean False, Boolean False, Boolean True]
                it "should make __lt work for (>) as well" $
                    runParse (unlines [
                         "t = { x = 4 }"
                        ,"setmetatable(t, { __lt = function(a,b) return b.x > a end })"
                        ,"return t > 3, t > 4, t > 5"
                    ]) `shouldBe` [Boolean True, Boolean False, Boolean False]

            describe "metatable equality" $ do
                it "should allow setting the __eq metafunction" $
                    runParse (unlines [
                         "t,u,v = {x=5}, {x=5}, {x=6}"
                        ,"e = function(t,u) return t.x == u.x end"
                        ,"mt = { __eq = e }"
                        ,"setmetatable(t, mt)"
                        ,"setmetatable(u, mt)"
                        ,"setmetatable(v, mt)"
                        ,"return t == t, t == u, t == v, t == {}, t == 42"
                    ]) `shouldBe` (map Boolean [True, True, False, False, False])

                it "should only call __eq if both are the same" $
                    runParse (unlines [
                         "t,u = {x=5}, {x=5}"
                        ,"e = function(t,u) return t.x == u.x end"
                        ,"f = function(t,u) return t.x == u.x end"
                        ,"setmetatable(t, { __eq = e })"
                        ,"setmetatable(u, { __eq = f })"
                        ,"return t == u"
                    ]) `shouldBe` ([Boolean False])

            describe "special table metafunctions" $ do
                it "should allow setting the __call metafunction" $
                    runParse (unlines [
                         "t = { x = 5 }"
                        ,"setmetatable(t, { __call = function(a, x) return a.x + x end })"
                        ,"return t(42)"
                        ]) `shouldBe` [Number 47.0]

                it "should allow setting the __tostring metafunction" $
                    runParse (unlines [
                         "t = { }"
                        ,"setmetatable(t, { __tostring = function(t) return \"t\" end })"
                        ,"return tostring(t)"
                        ]) `shouldBe` [Str "t"]

                it "should allow setting the __index metafunction" $
                    runParse (unlines [
                         "t = { }"
                        ,"setmetatable(t, { __index = function(t, i) return i end })"
                        ,"return t[1], t.x"
                        ]) `shouldBe` [Number 1.0, Str "x"]

                it "should allow setting the table as __index metafield" $
                    runParse (unlines [
                         "t = { }"
                        ,"u = { x = 33 }"
                        ,"setmetatable(t, { __index = u })"
                        ,"return t.x"
                        ]) `shouldBe` [Number 33.0]

                it "should prefer local key to the __index function" $
                    runParse (unlines [
                         "t = { x = 4 }"
                        ,"setmetatable(t, { __index = function(t, i) return i end })"
                        ,"return t.x"
                        ]) `shouldBe` [Number 4.0]

                it "should prefer local key to the __index metatable" $
                    runParse (unlines [
                         "t = { x = 4 }"
                        ,"u = { x = 5 }"
                        ,"setmetatable(t, { __index = u })"
                        ,"return t.x"
                        ]) `shouldBe` [Number 4.0]

                it "should allow setting the __newindex function" $
                    runParse (unlines [
                         "t = { }"
                        ,"setmetatable(t, { __newindex = function(t, k, v) rawset(t, k+1, v) end })"
                        ,"t[1] = 2.0"
                        ,"return t[1], t[2]"
                    ]) `shouldBe` [Nil, Number 2.0]

                it "shouldn't call __newindex if the key is already present" $
                    runParse (unlines [
                         "t = { x = Nil, y = 3.0}"
                        ,"setmetatable(t, { __newindex = function(t, k, v) rawset(t, k, v+1) end })"
                        ,"t.x = 11"
                        ,"t.y = 13"
                        ,"return t.x, t.y"
                    ]) `shouldBe` [Number 11.0, Number 13.0]

                describe "__metatable" $ do
                    it "should allow setting the __metatable hider" $
                        runParse (unlines [
                             "t = {}"
                            ,"setmetatable(t, { x = 3, __metatable = { x = 4 }})"
                            ,"return getmetatable(t).x"
                        ]) `shouldBe` [Number 4.0]

                    it "should prevent clearing the metatable if a hider is present" $
                        runParseFail (unlines [
                             "t = {}"
                            ,"setmetatable(t, { __metatable = { }})"
                            ,"setmetatable(t,nil)"
                        ]) `shouldSatisfy` (\[Str err] -> "Cannot change a protected metatable" `isPrefixOf` err)

                    it "should prevent changing the metatable if a hider is present" $
                        runParseFail (unlines [
                             "t = {}"
                            ,"setmetatable(t, { __metatable = { }})"
                            ,"setmetatable(t, { })"
                        ]) `shouldSatisfy` (\[Str err] -> "Cannot change a protected metatable" `isPrefixOf` err)

                it "should allow setting __pairs" $
                    runParse (unlines [
                         "t={}"
                        ,"setmetatable(t, { __pairs = function() return 42 end })"
                        ,"return pairs(t)"
                    ]) `shouldBe` [Number 42]

                it "should allow setting __ipairs" $
                    runParse (unlines [
                         "t={}"
                        ,"setmetatable(t, { __ipairs = function() return 42 end })"
                        ,"return ipairs(t)"
                    ]) `shouldBe` [Number 42]

main :: IO ()
main = hspec spec
