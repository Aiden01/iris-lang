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
import           Text.PrettyPrint.ANSI.Leijen


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

data SourceSpan = SourceSpan { begin, end :: SourcePos }

instance Semigroup SourceSpan where
  SourceSpan (SourcePos file0 line0 end0) (SourcePos _ line1 end1) <> SourceSpan (SourcePos file2 line2 end2) (SourcePos _ line3 end3) = SourceSpan (SourcePos (file0 <> file2) (line0 <> line2) (end0 <> end2)) (SourcePos (file0 <> file2) (line1 <> line3) (end1 <> end3))
type Expr = Cofree ExprF SourceSpan

data UnaryOp
    = Not
    | Negate deriving (Show)

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
    | Greater deriving (Show)



data Param = Param String Type

data StmtContext = InFunction | TopLevel

data Statement
    = VarDecl String (Maybe Type) Expr
    | FnDecl String [Param] [Statement] Type
    | IfStmt Expr  [Statement]  (Maybe [Statement])
    | WhileStmt Expr [Statement]
    | CallStmt String [Expr]
    | Assign String Expr StmtContext
    | ReturnStmt Expr
    | ForStmt String Expr [Statement]
    | UseStmt String

data Program = Program [Statement]
