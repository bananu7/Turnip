module Turnip.AST where

import Text.ParserCombinators.Parsec.Pos
import Data.List(intercalate)

type Name = String
type HasEllipsis = Bool

data LValue = LVar Name
          | LFieldRef Expr Expr
            deriving (Show, Eq)

data UnaryOperator = OpLength
                   | OpNot
                   | OpUnaryMinus

data BinaryOperator = OpRaise
                    | OpMult
                    | OpDivide
                    | OpModulo
                    | OpPlus
                    | OpMinus
                    | OpConcat
                    | OpLE
                    | OpGE
                    | OpLess
                    | OpGreater
                    | OpEqual
                    | OpNotEqual
                    | OpAnd
                    | OpOr

data Expr = Number Double
          | StringLiteral SourcePos String
          | Bool Bool
          | Nil
          | Ellipsis
          | Call Expr [Expr]
          | MemberCall Expr Name [Expr]
          | TableCons [(Maybe Expr, Expr)]
          | UnOp Operator Expr
          | BinOp Operatpr Expr Expr
          | FieldRef Expr Expr
          | Var Name
          | Lambda [Name] HasEllipsis Block
          deriving (Show, Eq)


data Stmt = Do Block
          | While Expr Block
          | Until Expr Block
          | If [(Expr, Block)] (Maybe Block)
          | Return [Expr]
          | Break
          | For [Name] ForGen Block
          | Assignment [LValue] [Expr]
          | LocalDecl [Name]
          | CallStmt Expr [Expr]
          | MemberCallStmt Expr Name [Expr]
          deriving Eq

instance Show Stmt where
    show (Do x) = show x
    show (Return xs) = "return " ++ show xs
    show (Assignment lvals exprs) = intercalate "\n" $ zipWith (\lval expr -> 
        (show lval) ++ " := " ++ (show expr)) lvals exprs
    show (CallStmt f params) = show f ++ "(" ++ show params ++ ")"
    show (MemberCallStmt obj f params) = show obj ++ ":" ++ f ++ "(" ++ show params ++ ")"
    show (If (block:elseIfBlocks) elseBlock) = "if " ++ show block ++ elseIfBlocksStr ++ elseBlockStr
      where
        elseIfBlocksStr = concatMap (("\nelseif " ++) . show) elseIfBlocks
        elseBlockStr = maybe "" (("\nelse " ++) . show) elseBlock
    show (LocalDecl names) = "local " ++ (intercalate "," names)

    show (For names (ForNum a b ms) block) = concat [
        "for ",
        (intercalate "," names),
        " = ",
        show a,
        ",",
        show b,
        stepStr,
        " do",
        show block
        ," end"
        ]
        where
          stepStr = case ms of
              Nothing -> ""
              Just s -> "," ++ show s

    show (For names (ForIter es) block) =
        "for " ++ (intercalate "," names) ++ " in " ++ (intercalate "," (map show es)) ++ " do" ++ show block ++ "end"

    show Break = "break"

data ForGen = ForNum Expr Expr (Maybe Expr) -- for var = min,max,step do
            | ForIter [Expr]                -- for x in y do
            deriving (Show, Eq)

data Block = Block [Stmt]
    deriving (Eq)

instance Show Block where
    show (Block stmts) = "\n" ++ (intercalate "\n" . map show $ stmts)

