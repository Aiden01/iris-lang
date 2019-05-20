module Language.Parser.Types
    ( ParserT
    )
where

import qualified Text.Megaparsec               as Mega
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )

type ParserT = Mega.Parsec Void Text
