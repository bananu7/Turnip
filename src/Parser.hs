module Parser( prettyLuaFromFile, loadAST, parseLua ) where

import LuaAS

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List
import Control.Monad (liftM)

-- Might be better to have a function that reads from the file, and sep function to do the parsing, 
-- thus separating the IO from the AST

--screw it, do it live
fromRight :: Either a b -> b
fromRight (Right b) = b

loadAST :: String -> IO Block
loadAST fname = do
    anAST <- parseFromFile program fname
    return $ fromRight anAST

parseLua :: String -> Either ParseError Block
parseLua text = parse program "" text

-- This is for parser testing
prettyLuaFromFile :: FilePath -> IO ()
prettyLuaFromFile fname = do
    input <- readFile fname
    putStr input
    case parse program fname input of
        Left err -> do
            putStr "parse error at "
            print err
        Right x -> print x

-- A program is a block of LUA -
program :: Parser Block
program = do
    whiteSpace
    r <- block
    eof
    return $ Block r

-- A block/chunk is a series of statements, optionally delimited by a semicolon -
block :: Parser [Stmt]
block = concat <$> many (regularStmt <|> localStmt)
  where 
    regularStmt :: Parser [Stmt]
    regularStmt = do
        s <- stat <|> laststat -- Not correct, could have many laststatements
        optional semi
        return [s]

-- Return will return some list of expressions, or an empty list of expressions. 
laststat :: Parser Stmt
laststat = do
    reserved "return"
    e <- option [] explist
    return $ Return e
  <|> do
    reserved "break"
    return Break

-- Make a choice to determine what kind of statement is being parsed
stat :: Parser Stmt
stat = choice [
    doStmt,
    whileStmt,
    repeatStmt,
    ifStmt,
    funcStmt,
    assignOrCallStmt
    ]

doStmt :: Parser Stmt
doStmt = do
    b <- between (reserved "do") (reserved "end") block
    return $ Do (Block b)

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    e <- expr
    b <- between (reserved "do") (reserved "end") block
    return $ While e (Block b)

repeatStmt :: Parser Stmt
repeatStmt = do
    reserved "repeat"
    b <- block
    reserved "until"
    e <- expr
    return $ Until e (Block b)

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    e <- expr
    reserved "then"
    b <- block

    elseIfBlocks <- many $ do
        reserved "elseif"
        e' <- expr
        reserved "then"
        b' <- block
        return (e', Block b')

    elseBlock <- optionMaybe $ do
        reserved "else"
        b' <- block
        return (Block b')

    reserved "end"
    return $ If ((e, Block b) : elseIfBlocks) elseBlock

-- |"function statement" is just syntax sugar over
--  assignment of a lambda
funcStmt :: Parser Stmt
funcStmt = do 
    reserved "function"
    fname <- funcname
    fparams <- paramList
    fbody <- funcBody

    case fname of
        (name, _) -> return $ Assignment [LVar name] [Lambda fparams fbody]
        -- TODO: proper resolution of LFieldRefs

-- |this is essentially a lambda expression
lambda :: Parser Expr
lambda = do 
    reserved "function"
    fparams <- paramList
    fbody <- funcBody
    return $ Lambda fparams fbody
    
paramList = parens (option [] namelist)

funcBody = do
    b <- block 
    reserved "end"
    return $ (Block b) 

{-
I've decided to desugar local statements to something much easier to eval. Thus:
   local x = 5
becomes
   local x
   x = 5
Which thanks to closure local scoping will be properly assigned.
This however requires localStmt to emit more than one statement
-}
localStmt :: Parser [Stmt]
localStmt = reserved "local" >> (localFuncStmt <|> localAssignStmt)
    where
        localFuncStmt = do
            reserved "function"
            -- regular function names aren't allowed here,
            -- because local functions can't be methods (with dots inside)
            fname <- identifier
            fparams <- paramList
            fbody <- funcBody
            optional semi
            return $ [
                LocalDecl [fname],
                Assignment [LVar fname] [Lambda fparams fbody]
                ]

        localAssignStmt = do
            names <- namelist
            symbol "="
            vals <- explist
            optional semi
            return $ [
                LocalDecl names,
                Assignment (map LVar names) vals
                ]

assignOrCallStmt :: Parser Stmt
assignOrCallStmt = do
    ex <- primaryexp
    case ex of 
        --Function
        (Call fn params) -> return $ CallStmt fn params
        (MemberCall _ _ _) -> return $ Assignment [] [ex]
        -- Assignment
        (Var n) -> assignStmt [LVar n]
        (FieldRef t f) -> assignStmt [LFieldRef t f]
        _ -> fail "Invalid stmt"

assignStmt lhs = do
    comma
    lv <- lvalue
    assignStmt (lv:lhs)
  <|> do
    symbol "="
    vals <- explist
    return $ Assignment (reverse lhs) vals

lvalue = do
    ex <- primaryexp
    tolvar ex

tolvar ex = do
    case ex of
        (Var n) -> return $ LVar n
        (FieldRef t f) -> return $ LFieldRef t f
        _ -> fail "Invalid lvalue"

namelist :: Parser [Name]
namelist = commaSep1 identifier

prefixexp = choice [
    identifier >>= return . Var,
    parens expr
    ]

args = (parens $ option [] explist)
    <|> (liftM (:[]) $ tableconstructor)
    <|> (getPosition >>= \pos -> liftM (\s -> [StringLiteral pos s]) $ stringl)

functioncall = do
    prefixexp
    args
  <|> do
    prefixexp
    colon
    identifier
    args
    
-- Function names are identifiers seperated by 0 or more dots, and with an optional colon, identifier at the end.
funcname :: Parser (Name, Maybe Name)
funcname = do
    n1 <- fmap (intercalate ".") (sepBy identifier dot)
    n2 <- optionMaybe (colon >> identifier)
    return (n1,n2)

explist :: Parser [Expr]
explist = commaSep1 expr

tableconstructor = liftM TableCons $ braces fieldlist

fieldlist = sepEndBy field fieldsep

field = do
    e <- brackets expr
    symbol "="
    v <- expr
    return (Just e, v)

  <|> do
    pos <- getPosition
    id <- try $ do 
        i <- identifier
        symbol "="
        return i
    v <- expr
    return (Just (StringLiteral pos id), v)

  <|> do
    v <- expr
    return (Nothing, v)

fieldsep = comma <|> semi

primaryexp = do
    pfx <- prefixexp
    more pfx
    where
        more i = (dot_index i >>= more)
             <|> (brace_index i >>= more)
             <|> (member_call i >>= more)
             <|> (fcall i >>= more)
             <|> return i

        dot_index e = do 
            dot
            pos <- getPosition
            id <- identifier
            return $ FieldRef e (StringLiteral pos id)
            
        brace_index e = liftM (FieldRef e) $ brackets expr
        
        member_call e = do
            colon
            id <- identifier
            arg <- args
            return $ MemberCall e id arg
            
        fcall e = liftM (Call e) args

exp_exp = choice [
    liftM Number $ number,
    getPosition >>= \pos -> liftM (StringLiteral pos) $ stringl,
    reserved "true" >> return (Bool True),
    reserved "false" >> return (Bool False),
    reserved "nil" >> return Nil,
    reserved "..." >> return Ellipsis,
    tableconstructor,
--    exp_anonfunction,
    primaryexp,
    lambda
    ]


--------------------------------------------
--Binary and Unary Expression parser
--------------------------------------------
expr :: Parser Expr
expr = buildExpressionParser optable exp_exp

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
number    = try (P.float lexer) <|> (fromIntegral <$> (P.integer lexer))
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