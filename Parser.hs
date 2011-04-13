module Parser( prettyLuaFromFile ) where

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

-- A program is a block of LUA --
program 
    = do{ whiteSpace
        ; r <- block
        ; eof
        ; return $ Block r
        }

-- A block/chunk is a series of statements, optionally delimited by a semicolon --
block
    = many1 (do{ s <- stat <|> laststat
        ; optional semi
        ; return s
        })

-- Return will return some list of expressions, or an empty list of expressions. 
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
    repeatStmt
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

-- Var list and name list are variables and identifiers separated by commas --
varlist = commaSep1 var
namelist = commaSep1 identifier
explist = commaSep1 exp_exp
-- A variable is either an identifier, a value of a certain index in a table, third option is syntactic sugar for table access--
var = do{ i <- identifier;
        ; return (Var i)
        }

--prefixexp = var | functioncall | `(´ exp `)´

--tableconstructor 
--    = do{ t <- braces (optional fieldlist)
--        ; return i;
--        }
--field ::= `[´ exp `]´ `=´ exp | Name `=´ exp | exp    


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

--funcname :: Parser String
--funcname = identifier option (many (char '.' identifier) (char ':' identifier))
--varlist = commaSep1 var
--var =  identifier  
--	<|> do prefixexp; brackets exp; 
--	<|> do prefixexp; dot; identifier
--namelist = commaSep1 identifier
--explist = do many (try (commaSep1 exp)); exp
--prefixexp = var <|> functioncall <|> (parens exp)
--functioncall = do prefixexp args <|> do prefixexp colon identifier args 
--args =  option (parens (explist)) <|> tableConstructor <|> String
--function = do reserved "function" funcbody
--funcbody = do option () (parens parlist) Block reserved "end"
--parlist = do option namelist (char ',' "...") <|> do String "..."
--tableConstructor = do option (braces (fieldlist))
--fieldlist = sepEndBy field fieldsep
--field = do (brackets exp) char '=' exp 
--		<|> identifier char '=' exp 
--		<|> exp
--
--fieldsep = comma <|> semi
--parse_file :: FilePath -> IO (Either ParseError Block)
--parse_file path = liftM (parse fileBlock path) (readFile path)
--		where fileBlock = do r <- Block; eof; return r
