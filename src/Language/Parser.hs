module Language.Parser
    ( parseFile
    )
where

import           Language.PrettyPrinter
import           Language.Parser.ProgramParser
import           Text.Megaparsec
import           Text.Megaparsec.Error
import qualified Data.Text                     as T

parseFile :: String -> IO ()
parseFile path = do
    contents <- T.pack <$> readFile path
    case parse (parseProgram <* eof) "" contents of
        Left  e   -> red $ errorBundlePretty e
        Right ast -> green (prettyProgram ast)
