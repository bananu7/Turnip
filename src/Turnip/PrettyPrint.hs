module Turnip.PrettyPrint where

import Turnip.AST as AST
import qualified Turnip.Eval.Types

import Data.List (intercalate)
import Numeric

class PrettyPrint a where
    prettyPrint :: a -> String

instance PrettyPrint Stmt where
    prettyPrint (Do x) = "do\n" ++ show x ++ "end\n"
    prettyPrint (While e b) = "while " ++ show e ++ " do\n" ++ show b ++ "end\n"
    prettyPrint (Until e b) = "until " ++ show e ++ " do\n" ++ show b ++ "end\n"
    prettyPrint (Return xs) = "return " ++ show xs
    prettyPrint (Assignment lvals exprs) = intercalate "\n" $ zipWith (\lval expr -> 
        (show lval) ++ " := " ++ (show expr)) lvals exprs
    prettyPrint (CallStmt f params) = show f ++ "(" ++ show params ++ ")"
    prettyPrint (MemberCallStmt obj f params) = show obj ++ ":" ++ f ++ "(" ++ show params ++ ")"
    prettyPrint (If block elseIfBlocks elseBlock) = "if " ++ show block ++ elseIfBlocksStr ++ elseBlockStr
      where
        elseIfBlocksStr = concatMap (("\nelseif " ++) . show) elseIfBlocks
        elseBlockStr = maybe "" (("\nelse " ++) . show) elseBlock
    prettyPrint (LocalDecl names) = "local " ++ (intercalate "," names)

    prettyPrint (For names (ForNum a b ms) block) = concat [
        "for ",
        (intercalate "," names),
        " = ",
        prettyPrint a,
        ",",
        prettyPrint b,
        stepStr,
        " do",
        prettyPrint block
        ," end"
        ]
        where
          stepStr = case ms of
              Nothing -> ""
              Just s -> "," ++ prettyPrint s

    prettyPrint (For names (ForIter es) block) =
        "for " ++ (intercalate "," names) ++ " in " ++ (intercalate "," (map prettyPrint es)) ++ " do" ++ prettyPrint block ++ "end"

    prettyPrint Break = "break"

instance PrettyPrint Expr where
    prettyPrint (Number n) = show n
    prettyPrint x = show x

instance PrettyPrint ForGen where
    prettyPrint = show

instance PrettyPrint Block where
    prettyPrint (Block stmts) = "\n" ++ (intercalate "\n" . map prettyPrint $ stmts)

-- for live values
instance PrettyPrint Turnip.Eval.Types.Value where
    prettyPrint (Turnip.Eval.Types.Number n) = showFFloat (Just 0) n ""
    prettyPrint x = show x

