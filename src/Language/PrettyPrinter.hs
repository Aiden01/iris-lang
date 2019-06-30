{-# LANGUAGE FlexibleInstances #-}
module Language.PrettyPrinter
  ( green
  , red
  )
where

import           System.Console.ANSI
import           Text.PrettyPrint.ANSI.Leijen   ( Pretty
                                                , pretty
                                                , integer
                                                , (<>)
                                                , (<+>)
                                                , Doc
                                                , text
                                                , double
                                                , char
                                                , bool
                                                , linebreak
                                                , indent
                                                , vsep
                                                , hsep
                                                )
import           Language.Parser.AST
import qualified Data.Text                     as T
import           Control.Comonad.Cofree
import           Language.Typing.Types

green :: Show a => a -> IO ()
green msg = do
  setSGR [SetColor Foreground Vivid Green]
  print msg
  setSGR [Reset]

red :: String -> IO ()
red msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn msg
  setSGR [Reset]

textWithColon :: String -> Doc
textWithColon s = text s <> text ": "

brackets, parens :: Doc -> Doc
brackets doc = text "[" <> linebreak <> doc <> linebreak <> text "]"
parens doc = text "(" <> doc <> text ")"

vsepIndent :: [Doc] -> Doc
vsepIndent = vsep . map (indent 2)


instance Pretty Lit where
  pretty (Number x) = textWithColon "Int" <> integer x
  pretty (Float x) = textWithColon "Float" <> double x
  pretty (Char' x) = textWithColon "Char" <> char x
  pretty (Str x) = textWithColon "String" <> text (T.unpack x)
  pretty (Array x) = textWithColon "Array" <> brackets (vsepIndent $ map pretty x)
  pretty (Object x) = textWithColon "Object"
  pretty (Boolean x) = textWithColon "Bool" <> bool x
  pretty Void = text "Void"

instance Pretty Expr where pretty (_ :< expr) = pretty expr

instance Pretty (ExprF Expr) where
  pretty (Literal lit) = pretty lit
  pretty (BinOp op expr expr') = textWithColon (show op) <> linebreak <> vsepIndent [pretty expr, pretty expr']
  pretty (UnaryOp op expr) = textWithColon (show op) <> pretty expr
  pretty (CallExpr name params) = textWithColon "Call" <> pretty name <+> text "params" <+> brackets (vsepIndent $ map pretty params)
  pretty (Var name) = textWithColon "Var" <> text name
  pretty (Lambda params expr) = textWithColon "λ" <> parens (hsep $ map text params) <+> text "->" <+> pretty expr
  pretty (Range expr' expr) = textWithColon "Range" <> text "From" <+> pretty expr' <+> text "To" <+> pretty expr

instance Pretty Param where
  pretty (Param name t) = textWithColon name <> pretty (show t)

instance Pretty Statement where
  pretty (VarDecl name (Just t) expr) = textWithColon ("val " <> name) <> text (show t) <+> text "=" <+> pretty expr
  pretty (Assign name expr _) = textWithColon "Assign" <> text name <+> text "=" <+> pretty expr
  pretty (IfStmt expr stmts elseBlock) = textWithColon "If" <> linebreak <> vsepIndent [textWithColon "then" <> pretty (Program stmts), textWithColon "else" <> pretty (Program <$> elseBlock)]
  pretty (WhileStmt expr stmts) = textWithColon "While" <> pretty expr <+> brackets (pretty $ Program stmts)
  pretty (FnDecl name params stmts t) = textWithColon "Fn" <> text name <> parens (hsep $ map (\ p -> pretty p <> text ",") params) <+> text (show t) <+> pretty (Program stmts)
  pretty (ReturnStmt expr) = textWithColon "Return" <> pretty expr
  pretty (ForStmt name expr stmts) = textWithColon "ForStmt" <> text name <+> text ":" <+> pretty expr <+> pretty (Program stmts)

instance Pretty Program where
  pretty (Program stmts) = textWithColon "Program" <> brackets (vsepIndent $ map pretty stmts)

instance Show Program where
    show = show . pretty
