
module Language.PrettyPrinter
    ( prettyProgram
    )
where

import           Language.Parser.AST
import           Text.PrettyPrint        hiding ( Str )
import           Prelude                 hiding ( (<>) )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M


prettyIdent :: String -> Doc
prettyIdent ident = text "=> " <> text ident <> text " <="

prettyBlock :: [TopLevelStatement] -> Doc
prettyBlock [] = text "[]"
prettyBlock stmts =
    text "[" $+$ nest 4 (vcat (map prettyStmt stmts)) <> text "\n]"

prettyProgram :: Program -> Doc
prettyProgram (Program []   ) = text "Program []"
prettyProgram (Program stmts) = text "Program" <> prettyBlock stmts


prettyStmt :: TopLevelStatement -> Doc
prettyStmt (VariableDeclaration name expr) =
    text "VarDecl"
        <>  prettyIdent name
        <>  text "["
        $+$ nest 4 (prettyExpr expr)
        <>  text "\n"
        $+$ nest 3 (text "]")

prettyStmt (FunctionDeclaration name params Nothing) =
    text "FunctionDecl"
        <> prettyIdent name
        <> text "("
        <> text (if null params then "()" else intercalate ", " params)
        <> text "[]"

prettyStmt (FunctionDeclaration name params (Just (Program stmts))) =
    text "FunctionDecl"
        <> prettyIdent name
        <> text "("
        <> text (if null params then "()" else intercalate ", " params)
        <> prettyBlock stmts

prettyStmt (IfCond expr (Just (Program stmts))) =
    text "If" <> text "( " <> prettyExpr expr <> text " )" <> prettyBlock stmts

prettyStmt (While expr (Just (Program stmts))) =
    text "While"
        <> text "( "
        <> prettyExpr expr
        <> text " )"
        <> prettyBlock stmts


prettyLit :: Lit -> Doc
prettyLit (Number  n   ) = text "Int " <> integer n
prettyLit (Str     str ) = text "String " <> text str
prettyLit (Char'   c   ) = text "Char " <> char c
prettyLit (Boolean bool) = text "Boolean " <> text (show bool)
prettyLit (Array exprs) =
    text "Array [" $+$ nest 4 (vcat (map prettyExpr exprs)) <> text "\n]"

prettyLit (Object obj) = text "Object [" $+$ nest 4 (prettyObject obj) <> text
    "\n]"
  where
    prettyObject :: M.Map String Expr -> Doc
    prettyObject =
        text
            . M.foldrWithKey
                  (\key val acc ->
                      (acc ++ key ++ ": " ++ show (prettyExpr val) ++ ",\n")
                  )
                  []

prettyBinOp :: BinOp -> Expr -> Expr -> Doc
prettyBinOp op expr1 expr2 =
    text (show op ++ " ( ")
        <> prettyExpr expr1
        <> text " )"
        <> text "( "
        <> prettyExpr expr2
        <> text " )"

prettyUnaryOp :: UnaryOp -> Expr -> Doc
prettyUnaryOp op expr = text (show op ++ " ( ") <> prettyExpr expr <> text " )"


prettyExpr :: Expr -> Doc
prettyExpr (Literal lit) = prettyLit lit
prettyExpr (BinOp binOp expr1 expr2) =
    text "Binary Op => " <> prettyBinOp binOp expr1 expr2
prettyExpr (UnaryOp unaryOp expr) =
    text "Unary Op => " <> prettyUnaryOp unaryOp expr
prettyExpr (Var name) = text "Var " <> text name
