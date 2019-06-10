module Language.Typing.Types
    ( ResultT
    )
where

import           Control.Monad.Except           ( ExceptT )
import           Language.Parser.AST            ( TypeExpr )
import           Language.Interpreter.Types     ( Value )

type ResultT = ExceptT TypeError IO ()
data TypeError = TypeError Value TypeError

instance Show TypeError where
    show (TypeError given expected) = "Mismatched types: expected " <> show expected <> ", but type " <> show given <> " was given."
