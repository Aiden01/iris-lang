module Language.Parser
    ( parseFile
    , parseBuffer
    , runIris
    )
where

import           Language.PrettyPrinter
import           Language.Parser.ProgramParser
import           Text.Megaparsec
import           Text.Megaparsec.Error
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )
import           Language.Parser.AST

runIris :: String -> IO ()
runIris = print . parseBuffer . T.pack

parseBuffer :: T.Text -> Either (ParseErrorBundle Text Void) Program
parseBuffer contents = parse (parseProgram <* eof) "" contents

parseFile :: String -> IO ()
parseFile path = T.pack <$> readFile path >>= print . parseBuffer
