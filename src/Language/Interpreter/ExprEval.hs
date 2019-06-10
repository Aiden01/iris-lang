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
                                                , liftIO
                                                )
import qualified Data.Text                     as T
import qualified Data.Map                      as M
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
evalExpr (CallExpr expr args) env = evalExpr expr env >>= \case
    Fn fn -> do
        args <- mapM (flip evalExpr env) args
        fn args
    _ -> throwError $ Custom "Mismatched types"

evalExpr (Lambda params expr) env =
    return $ Fn $ (\args -> invokeFn expr args params env)
evalExpr (Range expr expr') env = do
    to   <- evalExpr expr env
    from <- evalExpr expr' env
    case (to, from) of
        (VInt   x, VInt y  ) -> return $ VList $ map VInt [x .. y]
        (VFloat x, VFloat y) -> return $ VList $ map VFloat [x .. y]
        (VInt x, VFloat y) ->
            return $ VList $ map VFloat [(fromIntegral x) .. y]
        (VFloat x, VInt y) ->
            return $ VList $ map VFloat [x .. (fromIntegral y)]
        (VChar x, VChar y) -> return $ VList $ map VChar [x .. y]
        _                  -> throwError $ Custom "Mismatched types"
invokeFn :: Expr -> [Value] -> [String] -> Scope Value -> VResult
invokeFn expr given expected env = if length given == length expected
    then do
        let fnEnv = foldr
                (\(name, value) env -> insert name value env :: Scope Value)
                env
                (zip expected given)
        evalExpr expr fnEnv
    else
        throwError
        $  Custom
        $  "Expected "
        <> show (length expected)
        <> " arguments but "
        <> show (length given)
        <> " were given"


getOp :: Scope Value -> String -> [Value] -> VResult
getOp env op = let (Fn fn) = env ! op in fn

evalAttrExpr :: Scope Value -> String -> Value -> VResult
evalAttrExpr env attr expr = getOp env "." [expr, VString attr]


evalBinOp :: Scope Value -> BinOp -> Value -> Value -> VResult
evalBinOp env Add    x            y            = getOp env "+" [x, y]
evalBinOp env Sub    x            y            = getOp env "-" [x, y]
evalBinOp env Mult   x            y            = getOp env "*" [x, y]
evalBinOp env Pow    x            y            = getOp env "^" [x, y]
evalBinOp env And    x            y            = getOp env "&&" [x, y]
evalBinOp env Or     x            y            = getOp env "||" [x, y]
evalBinOp env Div    x            y            = getOp env "/" [x, y]
evalBinOp env Lower  x            y            = getOp env "<" [x, y]
evalBinOp env Concat x            y            = getOp env "++" [x, y]
evalBinOp env At (VList list) (VInt index) = return (list !! fromInteger index)

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


