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
        describe "literals" $ do
            it "should parse integers" $ parse "return 5" `shouldBe` Block [Return [Number 5.0]]
            it "should parse floats" $ parse "return 4.2" `shouldBe` Block [Return [Number 4.2]]
            it "should parse negative numbers" $ do
                parse "return -3" `shouldBe` Block [Return [UnOp "-" (Number 3.0)]]
                parse "return -2.9" `shouldBe` Block [Return [UnOp "-" (Number 2.9)]]
            -- this one can't be easily done because of the SourcePos
            --it "should parse strings" $ parse "\"test\"" `shouldBe` [StringLiteral 0 "test"]

        it "should parse simple assignments" $ do
            parse "x = 5" `shouldBe` (Block [Assignment [LVar "x"] [Number 5.0]])
            parse "x = y" `shouldBe` (Block [Assignment [LVar "x"] [Var "y"]])
            parse "f = function() end" `shouldBe` Block [Assignment [LVar "f"] [Lambda [] $ Block []]]

        it "should parse multiple assignments" $ do
            parse "x,y=1,2" `shouldBe` (Block [Assignment [LVar "x", LVar "y"] [Number 1.0, Number 2.0]])
            parse "a,b,c = 1,2" `shouldBe` (Block [Assignment [LVar "a", LVar "b", LVar "c"] [Number 1.0, Number 2.0]])
            parse "a,b = 1,2,3" `shouldBe` (Block [Assignment [LVar "a", LVar "b"] [Number 1.0, Number 2.0, Number 3.0]])

        it "should parse assignments using tables" $ do
            parse "t[i] = v" `shouldBe` (Block [Assignment [LFieldRef (Var "t") (Var "i")] [Var "v"]])
            parse "t[u[i]] = v" `shouldBe` (Block [Assignment [LFieldRef (Var "t") (FieldRef (Var "u") (Var "i"))] [Var "v"]])
            parse "t[i][u] = v" `shouldBe` Block [Assignment [LFieldRef (FieldRef (Var "t") (Var "i")) (Var "u")] [Var "v"]]

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

        describe "should parse function calls" $ do
            it "as a statement" $ do
                parse "f()" `shouldBe` (Block [CallStmt (Var "f") []])
                parse "f(1)" `shouldBe` (Block [CallStmt (Var "f") [Number 1.0]])
                parse "f(x,y)" `shouldBe` (Block [CallStmt (Var "f") [Var "x", Var "y"]])
            it "as an expression" $ do
                parse "return f()" `shouldBe` (Block [Return [Call (Var "f") []]])

        describe "should parse member calls" $ do
            it "as a statement" $ do
                parse "t:f()" `shouldBe` (Block [MemberCallStmt (Var "t") "f" []])
            it "as an expression" $ do
                parse "return t:f()" `shouldBe` (Block [Return [MemberCall (Var "t") "f" []]])

        it "should parse if statements" $ do
            parse "if true then return true end" `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]])] Nothing])
            parse "if true then return true else return false end"
                `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]])] (Just $ Block [Return [Bool False]])])
            parse "if true then return true elseif false then return false end"
                `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]]), (Bool False, Block [Return [Bool False]])] Nothing])

        it "should parse local definitions" $ do
            parse "local x = 5" `shouldBe` Block [LocalDecl ["x"], Assignment [LVar "x"] [Number 5.0]]
            parse "local a,b = 1,2" `shouldBe` Block [LocalDecl ["a", "b"], Assignment [LVar "a", LVar "b"] [Number 1.0, Number 2.0]]
            parse "local function f() end" `shouldBe` Block [LocalDecl ["f"], Assignment [LVar "f"] [Lambda [] (Block [])]]

        describe "loops" $ do
            it "should parse while loops" $ do
                parse "while true do end" `shouldBe` Block [While (Bool True) (Block [])]
                parse "while x < 1 do break end" `shouldBe`
                    Block [
                        While (BinOp "<" (Var "x") (Number 1.0)) (Block [
                            Break
                        ])
                    ]

            it "should parse numeric-for loops" $ do
                parse "for x = 1,2 do end" `shouldBe` Block [For ["x"] (ForNum (Number 1.0) (Number 2.0) Nothing) (Block [])]
                parse "for x = 5,1,-1 do break end" `shouldBe`
                    Block [
                        For ["x"] (ForNum (Number 5.0) (Number 1.0) (Just $ UnOp "-" (Number 1.0))) (Block [
                            Break
                        ])
                    ]

            it "should parse generic-for loops" $ do
                parse "for x in y do end" `shouldBe` Block [For ["x"] (ForIter [Var "y"]) (Block [])]
                parse "for k,v in pairs(t) do end" `shouldBe`
                    Block [
                        For ["k", "v"] (ForIter [Call (Var "pairs") [Var "t"]]) (Block [])
                    ]

main = hspec spec
