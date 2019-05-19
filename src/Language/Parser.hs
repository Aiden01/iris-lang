module Language.Parser
    ( module Exports
    , parseFile
    )
where

import           Language.Parser.Lexer         as Exports
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import           Text.ParserCombinators.Parsec
import           Language.Parser.AST
import           Language.Parser.Lexer
import qualified Data.Map                      as M
import           Text.ParserCombinators.Parsec.Expr
import           Language.PrettyPrinter



parseFile :: String -> IO ()
parseFile path = do
    contents <- readFile path
    case parse (parseProgram <* eof) "" contents of
        Left  e   -> red e
        Right ast -> green (prettyProgram ast)


parseProgram :: Parser Program
parseProgram = Program <$> (whiteSpace >> many (lexeme parseTopLevelStatement))

parseTopLevelStatement :: Parser TopLevelStatement
parseTopLevelStatement =
    try parseFunction
        <|> parseVariable
        <|> parseIfCond
        <|> parseWhile
        <|> parseFunctionCallTop

parseFunction :: Parser TopLevelStatement
parseFunction = do
    reserved "func"
    name   <- identifier
    params <- parens (commaSep identifier)
    block  <- braces (optionMaybe parseProgram)
    return (FunctionDeclaration name params block)

parseFunctionCallTop :: Parser TopLevelStatement
parseFunctionCallTop = do
    (fn, params) <- lexeme parseFunctionCall
    return (FunctionCallTop fn params)

parseFunctionCallExpr :: Parser Expr
parseFunctionCallExpr = do
    (fn, params) <- lexeme parseFunctionCall
    return (FunctionCallExpr fn params)

parseFunctionCall :: Parser (String, [Expr])
parseFunctionCall = do
    fn     <- identifier
    params <- parens (commaSep parseExpr)
    return (fn, params)

parseInt :: Parser Lit
parseInt = Number <$> integer

parseFloat :: Parser Lit
parseFloat = Float <$> float

parseString :: Parser Lit
parseString = Str <$> stringLiteral

parseBool :: Parser Lit
parseBool =
    Boolean <$> ((True <$ reserved "True") <|> (False <$ reserved "False"))

parseChar :: Parser Lit
parseChar = Char' <$> charLiteral

parseArray :: Parser Lit
parseArray = do
    exprs <- squareBrackets (commaSep parseExpr)
    return (Array exprs)

parseObject :: Parser Lit
parseObject = do
    entries <- braces (commaSep parseObjectEntry)
    return $ Object (M.fromList entries)
  where
    parseObjectEntry = do
        key <- identifier
        colon
        value <- parseExpr
        return (key, value)


parseLiteral :: Parser Lit
parseLiteral =
    parseChar
        <|> parseString
        <|> try parseFloat
        <|> parseInt
        <|> parseArray
        <|> parseObject
        <|> parseBool



parseExpr :: Parser Expr
parseExpr = buildExpressionParser opTable term <?> "expression"
  where
    opTable =
        [ [Prefix (reservedOp "-" >> return (UnaryOp Negate))]
        , [Prefix (reservedOp "not" >> return (UnaryOp Not))]
        , [Infix (reservedOp "and" >> return (BinOp And)) AssocLeft]
        , [Infix (reservedOp "&&" >> return (BinOp And)) AssocLeft]
        , [Infix (reservedOp "or" >> return (BinOp Or)) AssocLeft]
        , [Infix (reservedOp "||" >> return (BinOp Or)) AssocLeft]
        , [Infix (reservedOp "+" >> return (BinOp Add)) AssocLeft]
        , [Infix (reservedOp "*" >> return (BinOp Mult)) AssocLeft]
        , [Infix (reservedOp "/" >> return (BinOp Div)) AssocLeft]
        , [Infix (reservedOp "-" >> return (BinOp Sub)) AssocLeft]
        ]

term :: Parser Expr
term =
    parens parseExpr
        <|> Literal
        <$> parseLiteral
        <|> try parseFunctionCallExpr
        <|> Var
        <$> identifier

parseIfCond :: Parser TopLevelStatement
parseIfCond = do
    reserved "if"
    cond  <- parens parseExpr
    block <- braces (optionMaybe parseProgram)
    return (IfCond cond block)

parseWhile :: Parser TopLevelStatement
parseWhile = do
    reserved "while"
    cond  <- parens parseExpr
    block <- braces (optionMaybe parseProgram)
    return (While cond block)

parseVariable :: Parser TopLevelStatement
parseVariable = do
    reserved "val"
    name <- identifier
    lexeme (string "->")
    expr <- parseExpr
    char ';'
    return (VariableDeclaration name expr)
