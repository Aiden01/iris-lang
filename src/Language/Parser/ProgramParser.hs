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
parseProgram =
    Program <$> (space >> Mega.many (lexeme $ parseStatement TopLevel))

parseStatement :: StmtContext -> ParserT Statement
parseStatement ctx =
    Mega.try parseFnDeclStmt
        <|> parseVarDeclStmt
        <|> Mega.try (parseAssignStmt ctx)
        <|> parseIfStmt ctx
        <|> parseWhileStmt ctx
        <|> parseCallStmt
        <|> parseForStmt ctx
        <|> parseUseStmt
        <|> parseReturnStatement

parseUseStmt :: ParserT Statement
parseUseStmt = UseStmt <$> (keyword "use" *> identifier <* symbol ";")

parseReturnStatement :: ParserT Statement
parseReturnStatement =
    ReturnStmt <$> (keyword "return" *> parseExpr <* symbol ";")


parseAssignStmt :: StmtContext -> ParserT Statement
parseAssignStmt ctx = do
    name <- identifier
    _    <- symbol "="
    expr <- parseExpr
    _    <- symbol ";"
    return $ Assign name expr ctx

parseFnDeclStmt :: ParserT Statement
parseFnDeclStmt = do
    keyword "fn"
    name       <- identifier
    params     <- parens (commaSep param)
    returnType <- colon *> parseTypeExpr
    block      <- (braces $ Mega.many $ lexeme $ parseStatement InFunction)
    return (FnDecl name params block returnType)
  where
    param :: ParserT Param
    param = do
        name     <- identifier
        typeExpr <- colon *> parseTypeExpr
        return (Param name typeExpr)

parseCallStmt :: ParserT Statement
parseCallStmt = do
    fn     <- identifier
    params <- parens (commaSep parseExpr)
    _      <- symbol ";"
    return (CallStmt fn params)


parseIfStmt :: StmtContext -> ParserT Statement
parseIfStmt ctx = do
    keyword "if"
    cond     <- parens parseExpr
    block    <- braces (Mega.many $ parseStatement ctx)
    elseStmt <- Mega.optional parseElseStmt
    return (IfStmt cond block elseStmt)
  where
    parseElseStmt :: ParserT [Statement]
    parseElseStmt = keyword "else" *> braces (Mega.many $ parseStatement ctx)

parseWhileStmt :: StmtContext -> ParserT Statement
parseWhileStmt ctx = do
    keyword "while"
    cond  <- parens parseExpr
    block <- braces (Mega.many $ parseStatement ctx)
    return (WhileStmt cond block)

parseForStmt :: StmtContext -> ParserT Statement
parseForStmt ctx = do
    keyword "for"
    (id, list) <- parens $ do
        id' <- keyword "val" *> identifier
        colon
        list' <- parseExpr
        return (id', list')
    block <- braces $ Mega.many (lexeme $ parseStatement ctx)
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
