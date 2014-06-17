module Test where

import Test.Hspec

import Parser
import LuaAS

successful (Right x) = x
successful (Left err) = error $ show err

parse = successful . parseLua

spec :: Spec
spec = do
    describe "Parser.parseLua" $ do
        it "should parse simple assignments" $ do
            (parse "x = 5") `shouldBe` (Block [Assignment [LVar "x"] [Number 5.0]])
        it "should parse simple comparisons" $ do
            mapM_ (\op -> (parse $ "return 1 " ++ op ++ " 2") 
                            `shouldBe`
                          (Block [Return [BinOp op (Number 1) (Number 2)]])
                  )
                  ["==", "~=", ">", "<", ">=", "<="]
                  
        it "should parse function definitions" $ do
            (parse "function f() end") `shouldBe` (Block [Assignment [LVar "f"] [Lambda [] (Block [])]])
            (parse "function f(x) end") `shouldBe` (Block [Assignment [LVar "f"] [Lambda ["x"] (Block [])]])
            (parse "function f() return 1 end") `shouldBe` (Block [Assignment [LVar "f"] [Lambda [] (Block [Return [Number 1]])]])


main = hspec spec