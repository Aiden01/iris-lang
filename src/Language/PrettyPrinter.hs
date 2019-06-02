module Language.PrettyPrinter
    ( green
    , red
    )
where

import           System.Console.ANSI

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
{-import           Language.Parser.AST
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M

import           Text.PrettyPrint.Leijen        ( text
                                                , indent
                                                , double
                                                , integer
                                                , (<>)
                                                , (<+>)
                                                , Doc
                                                , char
                                                , vcat
                                                , linebreak
                                                , vsep
                                                , double
                                                )
import           Data.Map                       ( toList )
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )

import qualified Data.Text                     as T



wrapBlock :: Doc -> Doc
wrapBlock doc = text "[" <> linebreak <> doc <> linebreak <> text "]"

prettyProgram :: Program -> Doc
prettyProgram (Program stmts) =
    text "Program " <> wrapBlock (vsep $ map (indent 2 . prettyStmt) stmts)

prettyExprPos :: ExprPos -> Doc
prettyExprPos (ExprPos expr _) = prettyExpr expr

prettyExpr :: Expr -> Doc
prettyExpr (Literal lit) = prettyLit lit
prettyExpr (BinOp binOp expr1 expr2) =
    text "Binary Op => " <> prettyBinOp binOp expr1 expr2
prettyExpr (UnaryOp unaryOp expr) =
    text "Unary Op => " <> prettyUnaryOp unaryOp expr
prettyExpr (Var name) = text "Var " <> text name
prettyExpr (CallExpr name args) =
    text "FnCall"
        <+> text name
        <+> text "("
        <>  (vsep $ map ((<+> text ",") . prettyExprPos) args)
        <>  text ")"

prettyBinOp :: BinOp -> Expr -> Expr -> Doc
prettyBinOp op expr1 expr2 =
    text (show op ++ " (")
        <> prettyExpr expr1
        <> text ")"
        <> text "("
        <> prettyExpr expr2
        <> text ")"

prettyUnaryOp :: UnaryOp -> Expr -> Doc
prettyUnaryOp op expr = text (show op ++ " (") <> prettyExpr expr <> text ")"

prettyLit :: Lit -> Doc
prettyLit (Number  n   ) = text "Int " <> integer n
prettyLit (Float   f   ) = text "Float " <> double f
prettyLit (Str     str ) = text "String " <> text (T.unpack str)
prettyLit (Char'   c   ) = text "Char " <> char c
prettyLit (Boolean bool) = text "Boolean " <> text (show bool)
prettyLit (Array exprs) =
    text "Array" <+> wrapBlock (vcat (map (indent 2 . prettyExpr) exprs))

prettyLit (Object entries) = text "Object"
    <+> wrapBlock (vsep $ map (indent 2 . prettyEntry) $ toList entries)
prettyEntry :: (String, Expr) -> Doc
prettyEntry (key, value) = indent 2 $ text key <> text ": " <> prettyExpr value


wrapProgramBlock :: Maybe Program -> Doc
wrapProgramBlock block =
    wrapBlock (fromMaybe (text "empty") $ (indent 2 . prettyProgram) <$> block)

prettyStmt :: Statement -> Doc
prettyStmt (VarDecl name t expr) =
    text ("VarDecl " ++ name)
        <+> text ":"
        <+> fromMaybe (text "unknown") (text <$> (show <$> t))
        <+> text "="
        <+> prettyExprPos expr

prettyStmt (FnDecl name args block) =
    text ("FnDecl " ++ name)
        <+> text "("
        <>  text
                (intercalate
                    ", "
                    (map (\(Param n t) -> n ++ " " ++ ": " ++ show t) args)
                )
        <>  text ")"
        <+> wrapProgramBlock block
prettyStmt (IfStmt expr block elseBlock) =
    text "IfStmt"
        <+> text "("
        <>  prettyExprPos expr
        <>  text ")"
        <+> wrapProgramBlock block
-}
