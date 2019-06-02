module Language.Parser
    ( parseBuffer
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
runIris = parseBuffer . T.pack

parseBuffer :: T.Text -> IO ()
parseBuffer contents = case parse (parseProgram <* eof) "" contents of
    Left  e   -> red $ errorBundlePretty $ e
    Right ast -> green ast
{-parseFile :: String -> IO ()
parseFile path = T.pack <$> readFile path >>= print . parseBuffer
-}
