module LuaAS where

type Name = String

data LValue = LVar Name
          | LFieldRef Expr Expr
            deriving Show

data Expr = Number Double
          | StringLiteral String
          | Bool Bool
          | Nil
--          | Expr Ellipsis Expr
          | Function [Name] Bool Block
          | Call Expr [Expr]
          | MemberCall Expr Name [Expr]
          | TableCons [(Maybe Expr, Expr)]
          | UnOp String Expr
          | BinOp String Expr Expr
          | FieldRef Expr Expr
          | Var Name
          deriving Show

--data BinOp
--    = Add
--    | Subtract
--    | Multiply
--    | Divide
--    deriving (Show)

data Stmt = Do Block
          | While Expr Block
          | Until Expr Block
          | If [(Expr, Block)] (Maybe Block)
          | Return [Expr]
          | Break
          | For [Name] ForGen Block
          | Assignment [LValue] [Expr]
          | LocalDef [Name] [Expr]
          deriving Show

data ForGen = ForNum Expr Expr (Maybe Expr)
            | ForIter [Expr]
            deriving Show

data Block = Block [Stmt]
    deriving Show

