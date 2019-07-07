{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FunctionalDependencies #-}
module Language.Typing.Types
  ( Type(..)
  , TypeCheckEnv(..)
  , tcExpected
  , tcEnv
  , tc
  , TypeError(..)
  , TypeCheck
  , TypeCheckable
  )
where

import           Control.Monad.Except           ( ExceptT )
import           Language.Interpreter.Types     ( Value
                                                , Scope
                                                )
import qualified Data.Map                      as M
import           Language.Parser.AST
import           Text.Megaparsec.Pos
import           Data.List                      ( intercalate )
import           Control.Lens
import           Control.Monad.Reader

instance Show Type where
    show TInt = "Int"
    show TString = "String"
    show TChar = "Char"
    show TFloat = "Float"
    show (TArray t) = "[" <> show t <> "]"
    show TBool = "Boolean"
    show VoidT = "Void"
    show TAny = "Any"
    show (Fn params t) = "(" <> intercalate ", " (map show params) <> ") -> " <> show t

data TypeCheckEnv = TypeCheckEnv
  { _tcExpected :: Maybe Type
  , _tcEnv :: Scope Type }
makeLenses ''TypeCheckEnv

type TypeCheck a = ReaderT TypeCheckEnv (ExceptT TypeError IO) a


class TypeCheckable a b | a -> b where
  tc :: a -> TypeCheck b


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
