{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}
module Language.Parser.AST
    ( Statement(..)
    , Lit(..)
    , Expr(..)
    , Program(..)
    , BinOp(..)
    , UnaryOp(..)
    , Param(..)
    , ExprF(..)
    , SourceSpan(..)
    , Type(..)
    , StmtContext(..)
    )
where

import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Text.Megaparsec
import           Text.Megaparsec.Pos
import           Control.Comonad.Cofree

data Type
    = TInt
    | TString
    | TChar
    | TFloat
    | TArray Type
    | TVar String
    | TBool
    | VoidT
    | Fn [Type] Type
    | TAny

instance Eq Type where
    _ == TAny = True
    TAny == _ = True
    TInt == TInt = True
    TString == TString = True
    TChar == TChar = True
    VoidT == VoidT = True
    TFloat == TFloat = True
    TArray x == TArray y = x == y
    TBool == TBool = True
    TVar x == TVar y = x == y
    Fn types t == Fn types' t' = types == types' && t == t'
    _ == _ = False

data Lit
    = Number Integer
    | Boolean Bool
    | Str T.Text
    | Array [Expr]
    | Char' Char
    | Object (M.Map String Expr)
    | Float Double
    | Void



data ExprF a
    = Literal Lit
    | BinOp BinOp a a
    | UnaryOp UnaryOp a
    | Var String
    | AttrExpr a String
    | CallExpr a [a]
    | Lambda [String] a
    | Range a a
    deriving (Functor)

data SourceSpan = SourceSpan { begin, end :: SourcePos } deriving (Semigroup)

type Expr = Cofree ExprF SourceSpan

data UnaryOp
    = Not
    | Negate

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
    | At
    | NotEq
    | Eq
    | Greater



data Param = Param String Type

data StmtContext = InFunction | TopLevel

data Statement
    = VarDecl String (Maybe Type) Expr
    | FnDecl String [Param] [Statement]
    | IfStmt Expr  [Statement]  (Maybe [Statement])
    | WhileStmt Expr [Statement]
    | CallStmt String [Expr]
    | Assign String Expr StmtContext
    | ReturnStmt Expr
    | ForStmt String Expr [Statement]
    | UseStmt String

data Program = Program [Statement]

instance Show Program where
    show _ = "Not implemented yet"
