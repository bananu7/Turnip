module LuaAS where

import Text.ParserCombinators.Parsec.Pos
import Data.List(intercalate)

type Name = String

data LValue = LVar Name
          | LFieldRef Expr Expr
            deriving (Show, Eq)

data Expr = Number Double 
          | StringLiteral SourcePos String
          | Bool Bool
          | Nil
          | Ellipsis
          | Call Expr [Expr]
          | MemberCall Expr Name [Expr]
          | TableCons [(Maybe Expr, Expr)]
          | UnOp String Expr
          | BinOp String Expr Expr
          | FieldRef Expr Expr
          | Var Name
          | Lambda [Name] Block
          deriving (Show, Eq)


data Stmt = Do Block
          | While Expr Block
          | Until Expr Block
          | If [(Expr, Block)] (Maybe Block)
          | Return [Expr]
          | Break
          | For [Name] ForGen Block
          | Assignment [LValue] [Expr]
          | LocalDef [Name] [Expr]
          | CallStmt Expr [Expr]
          deriving Eq

instance Show Stmt where
    show (Do x) = show x
    show (Return xs) = "return " ++ show xs
    show (Assignment lvals exprs) = intercalate "\n" $ zipWith (\lval expr -> 
        (show lval) ++ " := " ++ (show expr)) lvals exprs
    show (CallStmt f params) = show f ++ "(" ++ show params ++ ")"


data ForGen = ForNum Expr Expr (Maybe Expr)
            | ForIter [Expr]
            deriving (Show, Eq)

data Block = Block [Stmt]
    deriving (Eq)

instance Show Block where
    show (Block stmts) = "\n" ++ (intercalate "\n" . map show $ stmts)

