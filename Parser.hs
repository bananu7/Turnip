module Parser( prettyLuaFromFile ) where

import Env
import LuaAS

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List
import Control.Monad (when, liftM)

prettyLuaFromFile fname
    = do{ input <- readFile fname
        ; putStr input
        ; case parse program fname input of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x
        }

-- A program is a block of LUA -
program 
    = do{ whiteSpace
        ; r <- block
        ; eof
        ; return $ Block r
        }

-- A block/chunk is a series of statements, optionally delimited by a semicolon --
block
    = many1 (do{ s <- stat <|> laststat -- Not correct, could have many laststatements
        ; optional semi
        ; return s
        })

-- Return will return some list of expressions, or an empty list of expressions. 
laststat :: Parser Stmt
laststat 
    = do{ reserved "return"
        ; e <- option [] explist
        ; return $ Return e
        }
  <|> do{ reserved "break"
        ; return Break
        }

-- Make a choice to determine what kind of statement is being parsed
stat :: Parser Stmt
stat = choice [
    doStmt,
    whileStmt,
    repeatStmt,
    ifStmt,
    funcStmt
--    simpleExpr
    ]

doStmt :: Parser Stmt
doStmt
    = do{ b <- between (reserved "do") (reserved "end") block
        ; return $ Do (Block b)
        }

whileStmt :: Parser Stmt
whileStmt
    = do{ reserved "while"
        ; e <- exp_exp
        ; b <- between (reserved "do") (reserved "end") block
        ; return $ While e (Block b)
        }

repeatStmt :: Parser Stmt
repeatStmt
    = do{ reserved "repeat"
        ; b <- block
        ; reserved "until"
        ; e <- exp_exp
        ; return $ Until e (Block b)
        }

ifStmt :: Parser Stmt
ifStmt
    = do{ reserved "if"
        ; e <- exp_exp
        ; reserved "then"
        ; b <- block 
        ; eb <- many $ do{ reserved "elseif"
                         ; e_ <- exp_exp
                         ; reserved "then"
                         ; b_ <- block
                         ; return (e_, Block b_)
                         }
        ; df <- optionMaybe $ do{reserved "else"; b <- block; return (Block b)}
        ; reserved "end"
        ; return $ If ((e, Block b):eb) df
        }

funcStmt :: Parser Stmt
funcStmt 
    = do{ reserved "function"
        ; funcname
        ; funcbody
        }

--simpleExpr :: Expr -> Stmt 
--simpleExpr = do{ e <- exp_exp; return Simple e}

-- Var list and name list are variables and identifiers separated by commas --
varlist = commaSep1 var

namelist = commaSep1 identifier

prefixexp = var
    <|> functioncall
    <|> parens exp_exp


args = parens (option [] explist)
    <|> tableconstructor
--    <|> stringl

functioncall = do{ prefixexp;args}
    <|> do{ prefixexp
          ; colon
          ; identifier
          ; args
          }
    
-- Function names are identifiers seperated by 0 or more dots, and with an optional colon, identifier at the end.
funcname 
    = do{ sepBy identifier dot 
        ; optional (do{colon;identifier})
        }

function = do{function; funcbody}

-- A function body has a parametr list (separated by commas and optionally terminated with an ellipsis) and also has a 
-- block for the main body.
funcbody :: Parser Stmt
funcbody
    = do{ par <- parens (option [] parlist)
        ; b <- block 
        ; reserved "end"
        ; return $ Function par (Block b)
        }

parlist = commaSep namelist 

explist = commaSep1 exp_exp

-- A variable is either an identifier, a value of a certain index in a table, third option is syntactic sugar for table access
var :: Parser Expr
var = do{ i <- identifier;
        ; return (Var i)
        }

tableconstructor = braces (option [] fieldlist)

fieldlist
    = do{ sepBy field fieldsep
       -- ; optional fieldsep
        }
field 
    = do{ brackets exp_exp
        ; symbol "="
        ; exp_exp 
        }
  <|> do{ identifier
        ; symbol "="
        ; exp_exp
        }
  <|> exp_exp

fieldsep = comma <|> semi

exp_exp :: Parser Expr
exp_exp = (reserved "nil" >> return (Nil))
    <|> (reserved "true" >> return (Bool True))
    <|> (reserved "false" >> return (Bool False))
    <|> expr

--------------------------------------------
--Binary and Unary Expression parser
--------------------------------------------
expr :: Parser Expr
expr = buildExpressionParser optable term

optable    = [ [Infix  (reservedOp "^"   >> return (BinOp "^")) AssocRight ]
             , [Prefix (reservedOp "-"   >> return (UnOp "-")) ]
             , [Infix  (reservedOp "*"   >> return (BinOp "*")) AssocLeft]
             , [Infix  (reservedOp "/"   >> return (BinOp "/"  )) AssocLeft]
             , [Infix  (reservedOp "%"   >> return (BinOp "%" )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (BinOp "+")) AssocLeft]
             , [Infix  (reservedOp "-"   >> return (BinOp "-")) AssocLeft]
             , [Infix  (reservedOp ".."  >> return (BinOp "..")) AssocRight]
             , [Infix  (reservedOp "<"   >> return (BinOp "<" )) AssocLeft]
             , [Infix  (reservedOp ">"   >> return (BinOp ">" )) AssocLeft]
             , [Infix  (reservedOp "<="  >> return (BinOp "<=")) AssocLeft]
             , [Infix  (reservedOp ">="  >> return (BinOp ">=")) AssocLeft]
             , [Infix  (reservedOp "~="  >> return (BinOp "~=")) AssocLeft]
             , [Infix  (reservedOp "=="  >> return (BinOp "==")) AssocLeft]
             , [Infix  (reservedOp "and" >> return (BinOp "and")) AssocLeft]
             , [Infix  (reservedOp "or"  >> return (BinOp "or")) AssocLeft] 
             ]

term =  parens expr
    <|> liftM Var identifier
    <|> liftM Number number

--------------------------------------------
-- The Lexer
--------------------------------------------	
lexer :: P.TokenParser()
lexer = P.makeTokenParser(
			emptyDef
			{reservedNames = ["end","in","repeat","break","false","local","return","do",
					"for","nil","then","else","function","not","true","elseif","if","until","while"],
			reservedOpNames = ["+","-","*","/","^","%","..","<","<=",">",">=","==","~=","and","or","not"]}
			)

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
stringl   = P.stringLiteral lexer
number    = P.float lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
comma     = P.comma lexer
braces    = P.braces lexer
brackets  = P.brackets lexer
dot       = P.dot lexer
colon     = P.colon lexer
commaSep1 = P.commaSep1 lexer
commaSep  = P.commaSep lexer
