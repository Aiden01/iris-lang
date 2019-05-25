module Language.Parser.AST
  ( Statement(..)
  , Lit(..)
  , Expr(..)
  , Program(..)
  , BinOp(..)
  , UnaryOp(..)
  , TypeExpr(..)
  , Param(..)
  )
where

import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import qualified Data.Text                     as T





data Lit
    = Number Integer
    | Boolean Bool
    | Str T.Text
    | Array [Expr]
    | Char' Char
    | Object (M.Map String Expr)
    | Float Double
    deriving (Show)



data Expr
    = Literal Lit
    | BinOp BinOp Expr Expr
    | UnaryOp UnaryOp Expr
    | Var String
    | CallExpr String [Expr]
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

data TypeExpr
  = TInt
  | TString
  | TChar
  | TFloat
  | VarT String
  deriving (Show)

data Param = Param String TypeExpr
  deriving (Show)

data Statement
    = VarDecl String (Maybe TypeExpr) Expr
    | FnDecl String [Param] (Maybe Program)
    | IfStmt Expr (Maybe Program) (Maybe [Statement])
    | WhileStmt Expr (Maybe Program)
    | CallStmt String [Expr]
    deriving (Show)

data Program = Program [Statement]
    deriving (Show)
