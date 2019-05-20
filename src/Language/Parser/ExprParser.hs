module Language.Parser.ExprParser
    ( parseExpr
    , parseCall
    )
where

import           Language.Parser.AST
import           Language.Parser.Lexer
import           Language.Parser.Types
import qualified Data.Map                      as M
import           Control.Applicative            ( (<|>) )
import qualified Text.Megaparsec               as Mega
import           Control.Monad.Combinators.Expr

parseCallExpr :: ParserT Expr
parseCallExpr = do
    (fn, params) <- lexeme parseCall
    return (CallExpr fn params)

parseCall :: ParserT (String, [Expr])
parseCall = do
    fn     <- identifier
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

parseObject :: ParserT Lit
parseObject = do
    entries <- braces (commaSep parseObjectEntry)
    return $ Object (M.fromList entries)
  where
    parseObjectEntry = do
        key <- identifier
        colon
        value <- parseExpr
        return (key, value)


parseLiteral :: ParserT Lit
parseLiteral =
    parseChar
        <|> parseString
        <|> Mega.try parseFloat
        <|> parseInt
        <|> parseArray
        <|> parseObject
        <|> parseBool



parseExpr :: ParserT Expr
parseExpr = makeExprParser term opTable
  where
    opTable =
        [ [Prefix (operator "-" >> return (UnaryOp Negate))]
        , [Prefix (operator "not" >> return (UnaryOp Not))]
        , [InfixL (operator "and" >> return (BinOp And))]
        , [InfixL (operator "&&" >> return (BinOp And))]
        , [InfixL (operator "or" >> return (BinOp Or))]
        , [InfixL (operator "||" >> return (BinOp Or))]
        , [InfixL (operator "+" >> return (BinOp Add))]
        , [InfixL (operator "*" >> return (BinOp Mult))]
        , [InfixL (operator "/" >> return (BinOp Div))]
        , [InfixL (operator "-" >> return (BinOp Sub))]
        ]

term :: ParserT Expr
term =
    parens parseExpr
        <|> Literal
        <$> parseLiteral
        <|> Mega.try parseCallExpr
        <|> Var
        <$> identifier

