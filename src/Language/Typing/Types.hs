{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Typing.Types
    ( TcState(..)
    , Type(..)
    , TypeEnv
    , TypeError(..)
    )
where

import           Control.Monad.Except           ( ExceptT )
import           Language.Interpreter.Types     ( Value
                                                , Scope
                                                )
import qualified Data.Map                      as M
import           Control.Monad.State
import           Language.Parser.AST
import           Text.Megaparsec.Pos



instance Show Type where
    show TInt = "Int"
    show TString = "String"
    show TChar = "Char"
    show TFloat = "Float"
    show (TArray t) = "[" <> show t <> "]"
    show TBool = "Boolean"
    show VoidT = "Void"

type TcState a = StateT [TypeEnv] (ExceptT TypeError IO) a
type TypeEnv = Scope Type


data TypeError
    = Mismatch Type Type SourceSpan
    | Redeclaration String SourceSpan
    | NotInScope String SourceSpan
    | Custom String SourceSpan

showPos :: SourceSpan -> String
showPos (SourceSpan pos _) = "Error in " <> sourcePosPretty pos

instance Show TypeError where
    show (Mismatch expected given pos) = showPos pos <> "\nmismatched types: expected " <> show expected <> ", but type " <> show given <> " was given."
    show (Redeclaration var pos) = showPos pos <> "\n" <> var <> " is already declared."
    show (NotInScope var pos) = showPos pos <> "\n" <> var <> " is not in scope."
    show (Custom str pos) = showPos pos <> "\n" <> str
