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

main = hspec spec
