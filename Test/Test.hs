module Main where

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
            parse "x = 5" `shouldBe` (Block [Assignment [LVar "x"] [Number 5.0]])
            parse "x = y" `shouldBe` (Block [Assignment [LVar "x"] [Var "y"]])

        it "should parse multiple assignments" $ do
            parse "x,y=1,2" `shouldBe` (Block [Assignment [LVar "x", LVar "y"] [Number 1.0, Number 2.0]])
            parse "a,b,c = 1,2" `shouldBe` (Block [Assignment [LVar "a", LVar "b", LVar "c"] [Number 1.0, Number 2.0]])
            parse "a,b = 1,2,3" `shouldBe` (Block [Assignment [LVar "a", LVar "b"] [Number 1.0, Number 2.0, Number 3.0]])

        it "should parse assignments using tables" $ do
            parse "t[i] = v" `shouldBe` (Block [Assignment [LFieldRef (Var "t") (Var "i")] [Var "v"]])
            parse "t[u[i]] = v" `shouldBe` (Block [Assignment [LFieldRef (Var "t") (FieldRef (Var "u") (Var "i"))] [Var "v"]])

        it "should parse simple comparisons" $ do
            mapM_ (\op -> (parse $ "return 1 " ++ op ++ " 2") 
                            `shouldBe`
                          (Block [Return [BinOp op (Number 1) (Number 2)]])
                  )
                  ["==", "~=", ">", "<", ">=", "<="]
                  
        it "should parse return statements" $ do
            parse "return false" `shouldBe` (Block [Return [Bool False]])
            parse "return 1, 2, 3" `shouldBe` (Block [Return [Number 1, Number 2, Number 3]])
            parse "return (42);" `shouldBe` (Block [Return [Number 42]])

        it "should parse function definitions" $ do
            parse "function f() end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda [] (Block [])]])
            parse "function f(x) end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda ["x"] (Block [])]])
            parse "function f() return 1 end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda [] (Block [Return [Number 1]])]])

        it "should parse function calls" $ do
            parse "f()" `shouldBe` (Block [CallStmt (Var "f") []])
            parse "f(1)" `shouldBe` (Block [CallStmt (Var "f") [Number 1.0]])
            parse "f(x,y)" `shouldBe` (Block [CallStmt (Var "f") [Var "x", Var "y"]])

        it "should parse if statements" $ do
            parse "if true then return true end" `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]])] Nothing])
            parse "if true then return true else return false end"
                `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]])] (Just $ Block [Return [Bool False]])])

main = hspec spec
