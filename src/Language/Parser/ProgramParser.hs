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
        <|> Mega.try parseAssignStmt
        <|> parseIfStmt
        <|> parseWhileStmt
        <|> parseCallStmt
        <|> parseForStmt

parseReturnStatement :: ParserT Statement
parseReturnStatement =
    ReturnStmt <$> (keyword "return" *> parseExpr <* symbol ";")


parseAssignStmt :: ParserT Statement
parseAssignStmt = do
    name <- identifier
    _    <- symbol "="
    expr <- parseExpr
    _    <- symbol ";"
    return $ Assign name expr

parseFnDeclStmt :: ParserT Statement
parseFnDeclStmt = do
    keyword "fn"
    name   <- identifier
    params <- parens (commaSep param)
    block  <-
        (braces $ Mega.optional $ Mega.many $ lexeme
            (parseStatement <|> parseReturnStatement)
        )
    return (FnDecl name params block)
  where
    param :: ParserT Param
    param = do
        name     <- identifier
        typeExpr <- Mega.optional (symbol ":" *> parseTypeExpr)
        return (Param name typeExpr)

parseCallStmt :: ParserT Statement
parseCallStmt = do
    fn     <- identifier
    params <- parens (commaSep parseExpr)
    _      <- symbol ";"
    return (CallStmt fn params)


parseIfStmt :: ParserT Statement
parseIfStmt = do
    keyword "if"
    cond     <- parens parseExpr
    block    <- braces (Mega.many parseStatement)
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

parseForStmt :: ParserT Statement
parseForStmt = do
    keyword "for"
    (id, list) <- parens $ do
        id' <- keyword "val" *> identifier
        colon
        list' <- parseExpr
        return (id', list')
    block <- braces $ Mega.many (lexeme parseStatement)
    return $ ForStmt id list block
parseVarDeclStmt :: ParserT Statement
parseVarDeclStmt = do
    keyword "val"
    name     <- identifier
    typeExpr <- Mega.optional (symbol ":" *> parseTypeExpr)
    symbol "="
    expr <- parseExpr
    symbol ";"
    return (VarDecl name typeExpr expr)
