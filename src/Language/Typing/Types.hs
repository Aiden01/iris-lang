module Language.Typing.Types
    ( ResultT
    , Type(..)
    , TypeEnv
    )
where

import           Control.Monad.Except           ( ExceptT )
import           Language.Interpreter.Types     ( Value )
import qualified Data.Map                      as M

data Type
    = TInt
    | TString
    | TChar
    | TFloat
    | TArray Type
    | TVar String
    | TBool
    | VoidT
    deriving (Show)

type TypeEnv = M.Map String Type

type ResultT = ExceptT TypeError IO ()
data TypeError = TypeError Value TypeError

instance Show TypeError where
    show (TypeError given expected) = "Mismatched types: expected " <> show expected <> ", but type " <> show given <> " was given."
