module Lib
    ( someFunc
    )
where

import           Language.Parser.Lexer
import qualified Text.ParserCombinators.Parsec.Token
                                               as T
import           Text.ParserCombinators.Parsec


someFunc :: IO ()
someFunc = putStrLn "It works"
