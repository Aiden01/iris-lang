{-# LANGUAGE RankNTypes, LiberalTypeSynonyms #-}
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
import           Language.Typing.Types
import           Control.Comonad.Cofree
import           Text.Megaparsec.Pos
import           Data.Void
import           Data.Function                  ( on )
import           Control.Comonad                ( extract )

type Parsed f = Cofree f SourceSpan

located :: ParserT (f (Cofree f SourceSpan)) -> ParserT (Cofree f SourceSpan)
located p = do
    s <- Mega.getSourcePos
    x <- p
    e <- Mega.getSourcePos
    pure (SourceSpan s e :< x)


parseCallExpr :: ParserT (Parsed ExprF)
parseCallExpr = located $ do
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

parseRange :: ParserT (Parsed ExprF)
parseRange = located $ brackets p
  where
    p = do
        from  <- parseLiteral <|> parseVarExpr <|> parens parseExpr
        range <- T.unpack <$> (symbol "..." <|> symbol "..")
        to    <- parseExpr
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


parseLiteral :: ParserT (Parsed ExprF)
parseLiteral = located $ Literal <$> litParser

litParser :: ParserT Lit
litParser =
  parseChar
            <|> parseString
            <|> Mega.try parseFloat
            <|> parseInt
            <|> parseArray
            <|> parseObject
            <|> parseBool

parseVarExpr :: ParserT (Parsed ExprF)
parseVarExpr = located $ Var <$> identifier

parseLambda :: ParserT (Parsed ExprF)
parseLambda = located parseLam
  where
    parseLam = do
        params <- parens (commaSep identifier)
        _      <- symbol "->"
        expr   <- parseExpr
        return $ Lambda params expr

binOp :: String -> BinOp -> ParserT (Expr -> Expr -> Expr)
binOp s op = operator s
    >> return (\lhs rhs -> ((<>) `on` extract) lhs rhs :< BinOp op lhs rhs)

unOp :: String -> UnaryOp -> ParserT (Expr -> Expr)
unOp s op = operator s >> return (\lhs -> extract lhs :< UnaryOp op lhs)


parseExpr :: ParserT (Parsed ExprF)
parseExpr = makeExprParser term opTable
  where
    opTable =
        [ [InfixL (binOp "++" Concat)]
        , [InfixL (binOp "at" At)]
        -- , [Postfix (operator "." >>= return (flip AttrExpr <$> identifier))]
        , [InfixL (binOp "==" Eq)]
        , [Prefix (unOp "-" Negate)]
        , [InfixL (binOp "!=" NotEq)]
        , [Prefix (unOp "not" Not)]
        , [InfixL (binOp "&&" And)]
        , [InfixL (binOp "||" Or)]
        , [InfixL (binOp "^" Pow)]
        , [InfixL (binOp "*" Mult)]
        , [InfixL (binOp "/" Div)]
        , [InfixL (binOp "+" Add)]
        , [InfixL (binOp "-" Sub)]
        , [InfixL (binOp "<" Lower)]
        , [InfixL (binOp ">" Greater)]
        ]

parsePattern :: ParserT (Parsed Pattern)
parsePattern = located (litPattern <|> PVar <$> identifier <|> symbol "_" *> pure PHole)
  where
    litPattern = PLit <$> litParser

parseMatchExpr :: ParserT (Parsed ExprF)
parseMatchExpr = located $ matchExpr
  where
    matchExpr = do
          keyword "match"
          expr <- parens parseExpr
          branches <- braces $ commaSep parseBranch
          return $ Match expr branches
    parseBranch = do
      p <- lexeme parsePattern
      symbol "->"
      expr <- parseExpr
      return (p, expr)

term :: ParserT Expr
term = lexeme
    (   Mega.try parseLambda
    <|> Mega.try parseRange
    <|> Mega.try parseLiteral
    <|> parens parseExpr
    <|> Mega.try parseCallExpr
    <|> parseVarExpr
    <|> parseMatchExpr
    )

parseTypeExpr, tInt, tString, tChar, tFloat :: ParserT Type
parseTypeExpr = lexeme
    (   Mega.try tInt
        <|> Mega.try tString
        <|> Mega.try tChar
        <|> Mega.try tArray
        <|> tFloat
        <|> tAny
        <|> tFn
        )
        Mega.<?> "type expression"

tInt = keyword "Int" *> pure TInt
tString = keyword "String" *> pure TString
tChar = keyword "Int" *> pure TChar
tFloat = keyword "Float" *> pure TFloat
tFn = do
  params <- parens $ commaSep parseTypeExpr
  symbol "->"
  returnT <- parseTypeExpr
  pure $ Fn params returnT

tArray = do
    keyword "Array"
    t0 <- brackets parseTypeExpr
    pure $ TArray t0
tAny = keyword "Any" *> pure TAny
