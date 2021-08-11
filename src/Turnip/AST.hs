module Turnip.AST where

import Text.ParserCombinators.Parsec.Pos

type Name = String
type HasEllipsis = Bool

data LValue = LVar Name
          | LFieldRef Expr Expr
            deriving (Show, Eq)

data UnaryOperator = OpLength
                   | OpNot
                   | OpUnaryMinus
                   deriving (Show, Eq)

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
                    deriving (Show, Eq)

data Expr = Number Double
          | StringLiteral SourcePos String
          | Bool Bool
          | Nil
          | Ellipsis
          | Call Expr [Expr]
          | MemberCall Expr Name [Expr]
          | TableCons [(Maybe Expr, Expr)]
          | UnOp UnaryOperator Expr
          | BinOp BinaryOperator Expr Expr
          | FieldRef Expr Expr
          | Var Name
          | Lambda [Name] HasEllipsis Block
          deriving (Show, Eq)

data Stmt = Do Block
          | While Expr Block
          | Until Expr Block
          | If (Expr, Block) [(Expr, Block)] (Maybe Block)
          | Return [Expr]
          | Break
          | For [Name] ForGen Block
          | Assignment [LValue] [Expr]
          | LocalDecl [Name]
          | CallStmt Expr [Expr]
          | MemberCallStmt Expr Name [Expr]
          deriving (Show,Eq)

data ForGen = ForNum Expr Expr (Maybe Expr) -- for var = min,max,step do
            | ForIter [Expr]                -- for x in y do
            deriving (Show, Eq)

data Block = Block [Stmt]
    deriving (Show, Eq)
