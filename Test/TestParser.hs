module Main where

import Test.Hspec

import Turnip.Parser
import Turnip.AST

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
            it "should parse strings" $ parse "return \"test\"" `shouldSatisfy` (\(Block [Return [StringLiteral _ s]]) -> s == "test")

        describe "should parse simple assignments" $ do
            it "a number to a variable" $
                parse "x = 5" `shouldBe` (Block [Assignment [LVar "x"] [Number 5.0]])

            it "a variable to another variable" $
                parse "x = y" `shouldBe` (Block [Assignment [LVar "x"] [Var "y"]])

            it "a lambda function to a variable" $
                parse "f = function() end" `shouldBe` Block [Assignment [LVar "f"] [Lambda [] False $ Block []]]

        describe "should parse multiple assignments" $ do
            it "equal arity of lhs and rhs" $
                parse "x,y=1,2" `shouldBe` (Block [Assignment [LVar "x", LVar "y"] [Number 1.0, Number 2.0]])

            it "larger lhs (not enough values)" $
                parse "a,b,c = 1,2" `shouldBe` (Block [Assignment [LVar "a", LVar "b", LVar "c"] [Number 1.0, Number 2.0]])

            it "larger rhs (too many values)" $
                parse "a,b = 1,2,3" `shouldBe` (Block [Assignment [LVar "a", LVar "b"] [Number 1.0, Number 2.0, Number 3.0]])

        describe "should parse assignments using tables" $ do
            it "simple (t[i] = v)" $
                parse "t[i] = v" `shouldBe` (Block [Assignment [LFieldRef (Var "t") (Var "i")] [Var "v"]])

            it "with key that's table access (t[u[i]] = v)" $
                parse "t[u[i]] = v" `shouldBe` (Block [Assignment [LFieldRef (Var "t") (FieldRef (Var "u") (Var "i"))] [Var "v"]])

            it "nested table (t[i][u] = v)" $
                parse "t[i][u] = v" `shouldBe` Block [Assignment [LFieldRef (FieldRef (Var "t") (Var "i")) (Var "u")] [Var "v"]]

        describe "should parse simple comparisons" $ do
            mapM_ (\op -> it op $ (parse $ "return 1 " ++ op ++ " 2") 
                            `shouldBe`
                          (Block [Return [BinOp op (Number 1) (Number 2)]])
                  )
                  ["==", "~=", ">", "<", ">=", "<="]
                  
        describe "should parse logical operators" $ do
            it "not" $
                parse "return not x" `shouldBe` (Block [Return [UnOp "not" (Var "x")]])
            it "or" $
                parse "return a or b" `shouldBe` (Block [Return [BinOp "or" (Var "a") (Var "b")]])
            it "and" $
                parse "return a and b" `shouldBe` (Block [Return [BinOp "and" (Var "a") (Var "b")]])

        describe "should parse return statements" $ do
            it "boolean" $ 
                parse "return false" `shouldBe` (Block [Return [Bool False]])

            it "a couple of numbers" $
                parse "return 1, 2, 3" `shouldBe` (Block [Return [Number 1, Number 2, Number 3]])

            it "some syntax noise" $
                parse "return (42);" `shouldBe` (Block [Return [Number 42]])

        describe "should parse function definitions" $ do
            it "empty named function" $
                parse "function f() end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda [] False (Block [])]])

            it "empty function with a parameter" $
                parse "function f(x) end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda ["x"] False (Block [])]])

            it "a function with a trivial return" $
                parse "function f() return 1 end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda [] False (Block [Return [Number 1]])]])

        describe "should parse function definitions with varargs" $ do
            it "only varargs" $
                parse "function f(...) end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda [] True (Block [])]])
            it "params and varargs" $
                parse "function f(x, ...) end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda ["x"] True (Block [])]])
            it "multiple params and varargs" $                
                parse "function f(x, y, z, ...) end" `shouldBe` (Block [Assignment [LVar "f"] [Lambda ["x", "y", "z"] True (Block [])]])

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

        describe "should parse if statements" $ do
            it "without else" $
                parse "if true then return true end" `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]])] Nothing])
            it "with else" $
                parse "if true then return true else return false end"
                    `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]])] (Just $ Block [Return [Bool False]])])
            it "with elseif" $
                parse "if true then return true elseif false then return false end"
                    `shouldBe` (Block [If [(Bool True, Block [Return [Bool True]]), (Bool False, Block [Return [Bool False]])] Nothing])

        describe "should parse local definitions" $ do
            it "simple local variable" $
                parse "local x = 5" `shouldBe` Block [LocalDecl ["x"], Assignment [LVar "x"] [Number 5.0]]

            it "multiple variables at the same time" $
                parse "local a,b = 1,2" `shouldBe` Block [LocalDecl ["a", "b"], Assignment [LVar "a", LVar "b"] [Number 1.0, Number 2.0]]

            it "local function syntax sugar" $
                parse "local function f() end" `shouldBe` Block [LocalDecl ["f"], Assignment [LVar "f"] [Lambda [] False (Block [])]]

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
