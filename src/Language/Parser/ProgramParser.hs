module Language.Parser.ProgramParser
  ( parseProgram
  , parseCall
  )
where

import           Language.Parser.AST
import           Language.Parser.Lexer
import           Language.Parser.Types
import qualified Text.Megaparsec               as Mega
import           Control.Applicative            ( (<|>) )
import           Language.Parser.ExprParser


parseProgram :: ParserT Program
parseProgram = Program <$> (space >> Mega.many (lexeme parseStatement))

parseStatement :: ParserT Statement
parseStatement =
  Mega.try parseFnDeclStmt
    <|> parseVarDeclStmt
    <|> parseIfStmt
    <|> parseWhileStmt
    <|> parseCallStmt


parseFnDeclStmt :: ParserT Statement
parseFnDeclStmt = do
  keyword "fn"
  name   <- identifier
  params <- parens (commaSep identifier)
  block  <- braces (Mega.optional parseProgram)
  return (FnDecl name params block)

parseCallStmt :: ParserT Statement
parseCallStmt = do
  (fn, params) <- lexeme parseCall
  symbol ";"
  return (CallStmt fn params)


parseIfStmt :: ParserT Statement
parseIfStmt = do
  keyword "if"
  cond    <- parens parseExpr
  block   <- braces (Mega.optional parseProgram)
  elseStmt <- Mega.optional parseElseStmt
  return (IfStmt cond block elseStmt)
 where
  parseElseStmt :: ParserT [Statement]
  parseElseStmt = keyword "else" *> braces (Mega.many parseStatement)

parseWhileStmt :: ParserT Statement
parseWhileStmt = do
  keyword "while"
  cond  <- parens parseExpr
  block <- braces (Mega.optional parseProgram)
  return (WhileStmt cond block)

parseVarDeclStmt :: ParserT Statement
parseVarDeclStmt = do
  keyword "val"
  name <- identifier
  symbol "="
  expr <- parseExpr
  symbol ";"
  return (VarDecl name expr)
