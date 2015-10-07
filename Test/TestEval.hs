module Main where

import Test.Hspec

import Parser (parseLua)
import qualified LuaAS as AST
import Eval

import Control.Monad.IO.Class

successful (Right x) = x
successful (Left err) = error $ show err

parse = successful . parseLua

runParse = run . parse

spec :: Spec
spec = do
    describe "Eval" $ do
        it "should eval return blocks" $
            runParse "return 1" `shouldBe` [Number 1.0]
        --it "should eval operator calls" $
        --    runParse "return 1 + 1" `shouldBe` [Number 2.0]

main = hspec spec
