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

main = hspec spec
