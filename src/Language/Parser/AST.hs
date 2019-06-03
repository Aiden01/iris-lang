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
import           Text.Megaparsec




data Lit
    = Number Integer
    | Boolean Bool
    | Str T.Text
    | Array [Expr]
    | Char' Char
    | Object (M.Map String Expr)
    | Float Double
    | Void
    deriving (Show)



data Expr
    = Literal Lit
    | BinOp BinOp Expr Expr
    | UnaryOp UnaryOp Expr
    | Var String
    | AttrExpr Expr String
    | CallExpr String [Expr]
    | ArrayIndex Expr Integer
    | Lambda [String] Expr
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
    | Pow
    | Lower
    | Concat
    deriving (Show)

data TypeExpr
  = TInt
  | TString
  | TChar
  | TFloat
  | VarT String
  deriving (Show)

data Param = Param String (Maybe TypeExpr)
  deriving (Show)

data Statement
    = VarDecl String (Maybe TypeExpr) Expr
    | FnDecl String [Param] (Maybe [Statement])
    | IfStmt Expr  [Statement]  (Maybe [Statement])
    | WhileStmt Expr (Maybe Program)
    | CallStmt String [Expr]
    | Assign String Expr
    | ReturnStmt Expr
    deriving (Show)

data Program = Program [Statement]
    deriving (Show)
