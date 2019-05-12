module Language.Parser.AST
  ( TopLevelStatement(..)
  , Lit(..)
  , Expr(..)
  , Program(..)
  , BinOp(..)
  , UnaryOp(..)
  )
where

import           Data.List                      ( intercalate )
import qualified Data.Map                      as M





data Lit
    = Number Integer
    | Boolean Bool
    | Str String
    | Array [Expr]
    | Char' Char
    | Object (M.Map String Expr)
    deriving (Show)


data Expr
    = Literal Lit
    | BinOp BinOp Expr Expr
    | UnaryOp UnaryOp Expr
    | Var String
    deriving (Show)

data UnaryOp
    = Not
    | Negate
    deriving (Show)

data BinOp
    = Add
    | Sub
    | Mult
    | And
    | Or
    | Div
    deriving (Show)

data TopLevelStatement
    = VariableDeclaration String Expr
    | FunctionDeclaration String [String] (Maybe Program)
    | IfCond Expr (Maybe Program)
    | While Expr (Maybe Program)
    deriving (Show)

data Program = Program [TopLevelStatement]
    deriving (Show)
