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
                                                , ExceptT
                                                )
import qualified Data.Text                     as T
import           Data.Map                      as M
import           Data.Map                       ( (!) )
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )
import           Language.PrettyPrinter
import           Control.Monad                  ( foldM )
import qualified Data.Set                      as Set


evalExpr :: Expr -> Scope Value -> VResult
evalExpr (Literal lit        ) env = evalLiteral lit env
evalExpr (BinOp op expr expr') env = do
    x <- evalExpr expr env
    y <- evalExpr expr' env
    evalBinOp env op x y
evalExpr (UnaryOp  op   expr) env = evalExpr expr env >>= evalUOp env op
evalExpr (AttrExpr expr attr) env = evalExpr expr env >>= evalAttrExpr env attr
evalExpr (Var x             ) env = case M.lookup x env of
    Just val -> return val
    Nothing  -> throwError $ Unbound x
evalExpr (CallExpr name args) env = case M.lookup name env of
    Just (Fn fn) -> do
        args <- mapM (flip evalExpr env) args
        fn args
    Nothing -> throwError $ Unbound name

getOp :: Scope Value -> String -> [Value] -> VResult
getOp env op = let (Fn fn) = env ! op in fn

evalAttrExpr :: Scope Value -> String -> Value -> VResult
evalAttrExpr env attr expr = getOp env "." [expr, VString attr]

evalBinOp :: Scope Value -> BinOp -> Value -> Value -> VResult
evalBinOp env Add    x y = getOp env "+" [x, y]
evalBinOp env Sub    x y = getOp env "-" [x, y]
evalBinOp env Mult   x y = getOp env "*" [x, y]
evalBinOp env Pow    x y = getOp env "^" [x, y]
evalBinOp env And    x y = getOp env "and" [x, y]
evalBinOp env Or     x y = getOp env "or" [x, y]
evalBinOp env Div    x y = getOp env "/" [x, y]
evalBinOp env Lower  x y = getOp env "<" [x, y]
evalBinOp env Concat x y = getOp env "++" [x, y]

evalUOp :: Scope Value -> UnaryOp -> Value -> VResult
evalUOp env Not    x = getOp env "!" [x]
evalUOp env Negate x = getOp env "-" [x]


evalLiteral :: Lit -> Scope Value -> VResult
evalLiteral (Number  x) _ = return $ VInt x
evalLiteral (Boolean x) _ = return $ VBool x
evalLiteral (Str     x) _ = return $ VString $ T.unpack x
evalLiteral (Char'   x) _ = return $ VChar x
evalLiteral (Array x) env =
    traverse (flip evalExpr env) x >>= \v -> return $ VList v
evalLiteral (Object x) env =
    traverse (flip evalExpr env) x >>= \v -> return $ VObject v
evalLiteral Void _ = return VoidV


