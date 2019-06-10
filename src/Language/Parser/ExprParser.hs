module Language.Parser.ExprParser
    ( parseExpr
    , parseCall
    , parseTypeExpr
    )
where

import           Language.Parser.AST
import           Language.Parser.Lexer
import           Language.Parser.Types
import qualified Data.Map                      as M
import           Control.Applicative            ( (<|>) )
import qualified Text.Megaparsec               as Mega
import qualified Text.Megaparsec.Char          as MegaC
import           Control.Monad.Combinators.Expr
import qualified Data.Text                     as T

parseCallExpr :: ParserT Expr
parseCallExpr = do
    (fn, params) <- lexeme parseCall
    return $ CallExpr fn params

parseCall :: ParserT (Expr, [Expr])
parseCall = do
    fn     <- parseVarExpr <|> parens (parseExpr)
    params <- parens (commaSep parseExpr)
    return (fn, params)


parseInt :: ParserT Lit
parseInt = Number <$> integerLiteral

parseFloat :: ParserT Lit
parseFloat = Float <$> floatLiteral

parseString :: ParserT Lit
parseString = Str <$> stringLiteral

parseBool :: ParserT Lit
parseBool =
    Boolean <$> ((True <$ keyword "True") <|> (False <$ keyword "False"))

parseChar :: ParserT Lit
parseChar = Char' <$> charLiteral

parseArray :: ParserT Lit
parseArray = do
    exprs <- brackets (commaSep parseExpr)
    return (Array exprs)

parseRange :: ParserT Expr
parseRange = brackets p
  where
    p = do
        from  <- parseLiteral <|> parseVarExpr <|> parens parseExpr
        range <- T.unpack <$> (symbol "..." <|> symbol "..")
        to    <- parseExpr >>= \expr -> case range of
            ".."  -> return $ BinOp Sub expr (Literal $ Number 1)
            "..." -> return expr
        return $ Range from to

parseObject :: ParserT Lit
parseObject = do
    entries <- braces (commaSep parseObjectEntry)
    return $ Object (M.fromList entries)
  where
    parseObjectEntry = do
        key <- (lexeme $ Mega.many MegaC.letterChar) Mega.<?> "letters"
        colon
        value <- parseExpr
        return (key, value)


parseLiteral :: ParserT Expr
parseLiteral = Literal <$> litParser
  where
    litParser :: ParserT Lit
    litParser =
        parseChar
            <|> parseString
            <|> Mega.try parseFloat
            <|> parseInt
            <|> parseArray
            <|> parseObject
            <|> parseBool

parseVarExpr :: ParserT Expr
parseVarExpr = Var <$> identifier

parseLambda :: ParserT Expr
parseLambda = do
    params <- parens (commaSep identifier)
    _      <- symbol "->"
    expr   <- parseExpr
    return $ Lambda params expr


parseExpr :: ParserT Expr
parseExpr = makeExprParser term opTable
  where
    opTable =
        [ [InfixL (operator "++" >> return (BinOp Concat))]
        , [InfixL (operator "at" >> return (BinOp At))]
        , [Postfix (operator "." >>= return (flip AttrExpr <$> identifier))]
        , [Prefix (operator "-" >> return (UnaryOp Negate))]
        , [Prefix (operator "not" >> return (UnaryOp Not))]
        , [InfixL (operator "&&" >> return (BinOp And))]
        , [InfixL (operator "||" >> return (BinOp Or))]
        , [InfixL (operator "^" >> return (BinOp Pow))]
        , [InfixL (operator "*" >> return (BinOp Mult))]
        , [InfixL (operator "/" >> return (BinOp Div))]
        , [InfixL (operator "+" >> return (BinOp Add))]
        , [InfixL (operator "-" >> return (BinOp Sub))]
        , [InfixL (operator "<" >> return (BinOp Lower))]
        ]


term :: ParserT Expr
term = lexeme
    (   Mega.try parseLambda
    <|> Mega.try parseRange
    <|> Mega.try parseLiteral
    <|> parens parseExpr
    <|> Mega.try parseCallExpr
    <|> parseVarExpr
    )

parseTypeExpr, tInt, tString, tChar, tFloat, varT :: ParserT TypeExpr
parseTypeExpr =
    (   Mega.try tInt
        <|> Mega.try tString
        <|> Mega.try tChar
        <|> Mega.try tFloat
        <|> Mega.try varT
        )
        Mega.<?> "type expression"

tInt = keyword "Int" *> pure TInt
tString = keyword "String" *> pure TString
tChar = keyword "Int" *> pure TChar
tFloat = keyword "Int" *> pure TFloat
varT = VarT <$> identifier
