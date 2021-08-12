{-# OPTIONS_GHC -Wno-missing-signatures #-} -- a lot of helpers
{-# OPTIONS_GHC -Wno-unused-do-bind #-} -- annoying discards of static data

module Turnip.Parser( prettyLuaFromFile, parseLua, block, explist) where

import Turnip.AST

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List
import Control.Monad (liftM)
import Data.Maybe

-- Might be better to have a function that reads from the file, and sep function to do the parsing, 
-- thus separating the IO from the AST

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
    forStmt,
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

forStmt :: Parser Stmt
forStmt = do
    reserved "for"
    vs <- namelist

    forGen <- do
        reserved "in"
        es <- explist
        return $ ForIter es
      <|> do
        symbol "="
        start <- expr
        comma
        stop <- expr
        step <- optionMaybe $ comma >> expr
        return $ ForNum start stop step

    b <- between (reserved "do") (reserved "end") block

    return $ For vs forGen (Block b)

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
    return $ If (e, Block b) elseIfBlocks elseBlock

-- |"function statement" is just syntax sugar over
--  assignment of a lambda
funcStmt :: Parser Stmt
funcStmt = do 
    reserved "function"
    fname <- funcname
    (fparams, varargs) <- paramList
    fbody <- funcBody

    case fname of
        (name, _) -> return $ Assignment [LVar name] [Lambda fparams varargs fbody]
        -- TODO: proper resolution of LFieldRefs

-- |this is essentially a lambda expression
lambda :: Parser Expr
lambda = do 
    reserved "function"
    (fparams, varargs) <- paramList
    fbody <- funcBody
    return $ Lambda fparams varargs fbody

paramList :: Parser ([Name], Bool)
paramList = parens $ do
    -- this can't be simply "namelist" because it will "eat" the last comma,
    -- and then fail on the "..."
    names <- many $ identifier <* optional comma
    varargs <- isJust <$> optionMaybe (string "...")
    return (names, varargs)

funcBody :: Parser Block
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
            (fparams, vararg) <- paramList
            fbody <- funcBody
            optional semi
            return $ [
                LocalDecl [fname],
                Assignment [LVar fname] [Lambda fparams vararg fbody]
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
        (MemberCall obj fn params) -> return $ MemberCallStmt obj fn params
        -- Assignment
        (Var n) -> assignStmt [LVar n]
        (FieldRef t f) -> assignStmt [LFieldRef t f]
        _ -> fail "Invalid stmt"

assignStmt :: [LValue] -> Parser Stmt
assignStmt lhs = do
    comma
    lv <- lvalue
    assignStmt (lv:lhs)
  <|> do
    symbol "="
    vals <- explist
    return $ Assignment (reverse lhs) vals

lvalue :: Parser LValue
lvalue = do
    ex <- primaryexp
    case ex of
        (Var n) -> return $ LVar n
        (FieldRef t f) -> return $ LFieldRef t f
        _ -> fail "Invalid lvalue"    

namelist :: Parser [Name]
namelist = commaSep1 identifier

prefixexp :: Parser Expr
prefixexp = choice [
    identifier >>= return . Var,
    parens expr
    ]

args :: Parser [Expr]
args = (parens $ option [] explist)
    <|> (liftM (:[]) $ tableconstructor)
    <|> (getPosition >>= \pos -> liftM (\s -> [StringLiteral pos s]) $ stringl)
    
-- Function names are identifiers seperated by 0 or more dots, and with an optional colon, identifier at the end.
funcname :: Parser (Name, Maybe Name)
funcname = do
    n1 <- fmap (intercalate ".") (sepBy identifier dot)
    n2 <- optionMaybe (colon >> identifier)
    return (n1,n2)

explist :: Parser [Expr]
explist = commaSep1 expr

tableconstructor :: Parser Expr
tableconstructor = liftM TableCons $ braces fieldlist

fieldlist :: Parser [(Maybe Expr, Expr)]
fieldlist = sepEndBy field fieldsep
    where
        fieldsep = comma <|> semi

field :: Parser (Maybe Expr, Expr)
field = do
    e <- brackets expr
    symbol "="
    v <- expr
    return (Just e, v)

  <|> do
    pos <- getPosition
    fieldId <- try $ do 
        i <- identifier
        symbol "="
        return i
    v <- expr
    return (Just (StringLiteral pos fieldId), v)

  <|> do
    v <- expr
    return (Nothing, v)

primaryexp :: Parser Expr
primaryexp = do
    pfx <- prefixexp
    more pfx
    where
        more i = (try (dot_index i >>= more))
             <|> (brace_index i >>= more)
             <|> (member_call i >>= more)
             <|> (fcall i >>= more)
             <|> return i

        dot_index e = do 
            dot
            pos <- getPosition
            fieldId <- identifier
            return $ FieldRef e (StringLiteral pos fieldId)
            
        brace_index e = liftM (FieldRef e) $ brackets expr
        
        member_call e = do
            colon
            memberId <- identifier
            arg <- args
            return $ MemberCall e memberId arg
            
        fcall e = liftM (Call e) args

exp_exp = choice [
    liftM Number $ number,
    getPosition >>= \pos -> liftM (StringLiteral pos) $ stringl,
    reserved "true" >> return (Bool True),
    reserved "false" >> return (Bool False),
    reserved "nil" >> return Nil,
    reserved "..." >> return Ellipsis,
    tableconstructor,
    primaryexp,
    lambda
    ]


--------------------------------------------
--Binary and Unary Expression parser
--------------------------------------------
expr :: Parser Expr
expr = buildExpressionParser optable exp_exp

optable    = [ [ Infix  (reservedOp "^"   >> return (BinOp OpRaise)) AssocRight
               ]
             , [ Prefix (reservedOp "not" >> return (UnOp OpNot))
               , Prefix (reservedOp "#"   >> return (UnOp OpLength))
               , Prefix (reservedOp "-"   >> return (UnOp OpUnaryMinus))
               ]
             , [ Infix  (reservedOp "*"   >> return (BinOp OpMult)) AssocLeft
               , Infix  (reservedOp "/"   >> return (BinOp OpDivide)) AssocLeft
               , Infix  (reservedOp "%"   >> return (BinOp OpModulo)) AssocLeft
               ]
             , [ Infix  (reservedOp "+"   >> return (BinOp OpPlus)) AssocLeft
               , Infix  (reservedOp "-"   >> return (BinOp OpMinus)) AssocLeft
               ]
             , [ Infix  (reservedOp ".."  >> return (BinOp OpConcat)) AssocRight
               ]
             , [ Infix  (reservedOp "<="  >> return (BinOp OpLE)) AssocLeft
               , Infix  (reservedOp ">="  >> return (BinOp OpGE)) AssocLeft
               , Infix  (reservedOp "~="  >> return (BinOp OpNotEqual)) AssocLeft
               , Infix  (reservedOp "=="  >> return (BinOp OpEqual)) AssocLeft
               , Infix  (reservedOp "<"   >> return (BinOp OpLess)) AssocLeft
               , Infix  (reservedOp ">"   >> return (BinOp OpGreater)) AssocLeft
               ]
             , [ Infix  (reservedOp "and" >> return (BinOp OpAnd)) AssocLeft
               ]
             , [ Infix  (reservedOp "or"  >> return (BinOp OpOr)) AssocLeft
               ]
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
-- lexeme    = P.lexeme lexer
symbol    = P.symbol lexer

-- Perhaps this should be implemented differently and not as a combination of builtin
-- and mine, but it seems to work just fine.
stringl   = try (P.stringLiteral lexer) <|> (singleQuotedLiteral)
    where
        singleQuotedLiteral :: Parser String
        singleQuotedLiteral = (char '\'' *> manyTill anyChar (char '\''))

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
-- commaSep  = P.commaSep lexer
