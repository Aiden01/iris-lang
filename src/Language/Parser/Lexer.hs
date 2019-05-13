module Language.Parser.Lexer
    ( lexeme
    , identifier
    , whiteSpace
    , lexer
    , reserved
    , parens
    , commaSep
    , braces
    , stringLiteral
    , charLiteral
    , integer
    , squareBrackets
    , colon
    , reservedOp
    , float
    )
where


import qualified Text.Parsec.Token             as Token
import           Text.Parsec.String             ( Parser )
import           Text.Parsec.Token              ( GenTokenParser
                                                , GenLanguageDef(..)
                                                , LanguageDef
                                                , TokenParser
                                                , makeTokenParser
                                                )
import           Text.Parsec.Language           ( emptyDef )
import           Text.Parsec

language :: LanguageDef ()
language = emptyDef
    { identStart      = letter <|> char '_'
    , identLetter     = identStart language
    , reservedNames   = ["func", "val", "if", "while", "True", "False"]
    , reservedOpNames = ["+", "-", "*", "/"]
    , commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , nestedComments  = True
    , caseSensitive   = True
    }

lexer :: TokenParser ()
lexer = Token.makeTokenParser language

reservedOp :: String -> Parser ()
reservedOp s = lexeme (Token.reservedOp lexer s)


identifier :: Parser String
identifier = lexeme (Token.identifier lexer)

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

parens :: Parser a -> Parser a
parens p = lexeme (Token.parens lexer p)

commaSep :: Parser a -> Parser [a]
commaSep p = lexeme (Token.commaSep lexer p)

colon :: Parser String
colon = lexeme (Token.colon lexer)

braces :: Parser a -> Parser a
braces = Token.braces lexer

squareBrackets :: Parser a -> Parser a
squareBrackets = Token.brackets lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

reserved :: String -> Parser ()
reserved str = lexeme (Token.reserved lexer str)

