{-# LANGUAGE LambdaCase #-}

module Language.Interpreter.ExprEval
    ( evalExpr
    )
where

import           Language.Parser.AST
import           Language.Interpreter.DefaultEnv
import           Language.Interpreter.Types
import           Text.Megaparsec.Error
import           Control.Monad.Except           ( runExceptT
                                                , throwError
                                                )
import qualified Data.Text                     as T
import           Data.Map                      as M
import           Data.Map                       ( (!) )
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )
import           Language.PrettyPrinter
import           Control.Monad                  ( foldM )
import qualified Data.Set                      as Set


evalExpr :: Either (ParseErrorBundle Text Void) Expr -> IO ()
evalExpr = \case
    Left  e -> red $ errorBundlePretty e
    Right r -> (runExceptT $ evalExpr' r defaultEnv) >>= print

evalExpr' :: Expr -> Scope Value -> VResult
evalExpr' (Literal lit        ) env = evalLiteral lit env
evalExpr' (BinOp op expr expr') env = do
    x <- evalExpr' expr env
    y <- evalExpr' expr' env
    evalBinOp env op x y
evalExpr' (UnaryOp op expr) env = evalExpr' expr env >>= evalUOp env op
evalExpr' (Var x          ) env = case M.lookup x env of
    Just val -> return val
    Nothing  -> throwError $ Unbound x
evalExpr' (CallExpr name args) env = case M.lookup name env of
    Just (Fn fn) -> do
        args <- mapM (flip evalExpr' env) args
        fn args
    Nothing -> throwError $ Unbound name


evalBinOp :: Scope Value -> BinOp -> Value -> Value -> VResult
evalBinOp env Add  x y = let (Fn fn) = env ! "+" in fn [x, y]
evalBinOp env Sub  x y = let (Fn fn) = env ! "-" in fn [x, y]
evalBinOp env Mult x y = let (Fn fn) = env ! "*" in fn [x, y]
evalBinOp env And  x y = let (Fn fn) = env ! "and" in fn [x, y]
evalBinOp env Or   x y = let (Fn fn) = env ! "or" in fn [x, y]
evalBinOp env Div  x y = let (Fn fn) = env ! "/" in fn [x, y]

evalUOp :: Scope Value -> UnaryOp -> Value -> VResult
evalUOp env Not    x = let (Fn fn) = env ! "!" in fn [x]
evalUOp env Negate x = let (Fn fn) = env ! "neg" in fn [x]


evalLiteral :: Lit -> Scope Value -> VResult
evalLiteral (Number  x) _   = return $ VInt x
evalLiteral (Boolean x) _   = return $ VBool x
evalLiteral (Str     x) _   = return $ VString $ T.unpack x
evalLiteral (Char'   x) _   = return $ VChar x
evalLiteral (Array   x) env = VList <$> traverse (flip evalExpr' env) x
evalLiteral Void        _   = return $ VoidV


