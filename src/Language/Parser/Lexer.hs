module Language.Parser.Lexer
    ( lexeme
    , identifier
    , space
    , keyword
    , parens
    , commaSep
    , braces
    , stringLiteral
    , charLiteral
    , integerLiteral
    , brackets
    , colon
    , operator
    , floatLiteral
    , symbol
    )
where


import qualified Text.Megaparsec               as Mega
import qualified Text.Megaparsec.Char          as MegaC
import qualified Text.Megaparsec.Char.Lexer    as MegaL
import           Language.Parser.Types          ( ParserT )
import qualified Data.Text                     as T
import qualified Data.Set                      as S


-- Execute the given parser and skip trailling spaces or comments
lexeme :: ParserT a -> ParserT a
lexeme = MegaL.lexeme space

-- Skips spaces and comments
space, skipLineComment, skipBlockComment :: ParserT ()
space = MegaL.space MegaC.space1 skipLineComment skipBlockComment
skipLineComment = MegaL.skipLineComment (T.pack "//")
skipBlockComment = MegaL.skipBlockComment (T.pack "/*") (T.pack "*/")

-- Parses a symbol, such as semicolon or comma
symbol :: String -> ParserT T.Text
symbol = MegaL.symbol space . T.pack


-- Parses a keyword
keyword :: String -> ParserT ()
keyword w = (lexeme . Mega.try)
    (MegaC.string (T.pack w) *> Mega.notFollowedBy MegaC.alphaNumChar)

-- Reserved keywords
keywords :: S.Set String
keywords = S.fromList
    [ "fn"
    , "val"
    , "if"
    , "while"
    , "True"
    , "False"
    , "else"
    , "Int"
    , "String"
    , "Char"
    , "Float"
    , "return"
    ]

-- Parses an identifier
identifier :: ParserT String
identifier = (lexeme . Mega.try) (p >>= check)
  where
    p = (:) <$> MegaC.letterChar <*> Mega.many MegaC.alphaNumChar
    check x
        | x `S.member` keywords
        = fail
            $  "Cannot use reserved keyword "
            ++ show x
            ++ " as an identifier."
        | otherwise
        = return x

-- Parses an operator
operator :: String -> ParserT T.Text
operator op | op `S.member` operators = lexeme $ MegaC.string (T.pack op)
            | otherwise               = fail $ "Unknown operator " ++ op
  where
    operators = S.fromList
        ["+", "-", "*", "/", "&&", "and", "or", "||", "^", "<", "++", "."]

-- Parses a char and skips trailling whitespace/comments
char :: Char -> ParserT Char
char = lexeme . MegaC.char

parens, braces, brackets :: ParserT a -> ParserT a
-- Parses what's inside the parentheses
parens = Mega.between (char '(') (char ')')
-- Parses what's inside braces
braces = Mega.between (char '{') (char '}')
-- Parses what's inside brackets
brackets = Mega.between (char '[') (char ']')

-- Parses parser p separated by zero or more commas
commaSep :: ParserT a -> ParserT [a]
commaSep p = Mega.sepBy p (symbol ",")

-- Parses a colon
colon :: ParserT T.Text
colon = lexeme (symbol ":")

charLiteral :: ParserT Char
charLiteral = MegaC.char '\'' *> MegaL.charLiteral <* MegaC.char '\''

stringLiteral :: ParserT T.Text
stringLiteral = T.pack <$> (MegaC.char '"' >> Mega.manyTill p (MegaC.char '"'))
  where
    p = Mega.label "valid string literal" $ do
        Mega.notFollowedBy (MegaC.char '\n')
        MegaL.charLiteral

integerLiteral :: ParserT Integer
integerLiteral = MegaL.decimal

floatLiteral :: ParserT Double
floatLiteral = MegaL.float
