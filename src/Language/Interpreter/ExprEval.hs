{-# LANGUAGE LambdaCase #-}

module Language.Interpreter.ExprEval
    ( evalExpr
    )
where

import           Language.Parser.AST     hiding ( Fn )
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
import           Control.Comonad.Cofree
import           Text.Megaparsec.Pos
import qualified Control.Monad.State           as S
import           Control.Applicative            ( (<|>) )

lookupScope :: String -> SourceSpan -> EvalState Value
lookupScope id pos =
    S.gets lookup' >>= maybe (throwError $ Unbound (begin pos) id) pure
  where
    lookup' :: [Scope Value] -> Maybe Value
    lookup' = foldr (<|>) Nothing . map (get id)

enterScope :: EvalState ()
enterScope = S.modify ((:) (M.fromList []))

leaveScope :: EvalState ()
leaveScope = S.modify tail

insertInScope :: String -> Value -> EvalState ()
insertInScope name t = S.modify (\(s : ss) -> insert name t s : ss)


evalExpr :: Expr -> EvalState Value
evalExpr (pos :< expr) = evalExpr' expr pos

evalExpr' :: ExprF (Expr) -> SourceSpan -> EvalState Value
evalExpr' (Literal lit                             ) pos = evalLiteral lit
evalExpr' (BinOp op (pos' :< expr) (pos'' :< expr')) pos = do
    x <- evalExpr' expr pos'
    y <- evalExpr' expr' pos''
    evalBinOp op x y
evalExpr' (UnaryOp op (pos :< expr)   ) pos' = evalExpr' expr pos >>= evalUOp op
-- evalExpr' (AttrExpr (pos :< expr) attr) pos' =
--     evalExpr' expr pos >>= evalAttrExpr attr
evalExpr' (Var x                      ) pos  = lookupScope x pos
evalExpr' (CallExpr (pos :< expr) args) pos' = evalExpr' expr pos >>= \case
    Fn fn -> do
        args <- mapM (\(aPos :< aExpr) -> evalExpr' aExpr aPos) args
        fn args

evalExpr' (Lambda params expr) pos =
    return $ Fn $ (\args -> invokeFn expr args params)
evalExpr' (Range (pos :< expr) (pos' :< expr')) pos''' = do
    to   <- evalExpr' expr pos
    from <- evalExpr' expr' pos'
    case (to, from) of
        (VInt   x, VInt y  ) -> return $ VList $ map VInt [x .. y]
        (VFloat x, VFloat y) -> return $ VList $ map VFloat [x .. y]
        (VInt x, VFloat y) ->
            return $ VList $ map VFloat [(fromIntegral x) .. y]
        (VFloat x, VInt y) ->
            return $ VList $ map VFloat [x .. (fromIntegral y)]
        (VChar x, VChar y) -> return $ VList $ map VChar [x .. y]
        _                  -> throwError $ Custom (begin pos) "Mismatched types"
invokeFn :: Expr -> [Value] -> [String] -> EvalState Value
invokeFn (pos :< expr) given expected = if length given == length expected
    then do
        enterScope
        mapM_ (\(name, value) -> insertInScope name value) (zip expected given)
        evalExpr' expr pos <* leaveScope
    else
        throwError
        $  Custom (begin pos)
        $  "Expected "
        <> show (length expected)
        <> " arguments but "
        <> show (length given)
        <> " were given"


getOp :: String -> EvalState ([Value] -> EvalState Value)
getOp op = (\(Fn fn) -> fn) <$> lookupScope op undefined

evalAttrExpr :: String -> Value -> EvalState Value
evalAttrExpr attr expr = getOp "." >>= \f -> f [expr, VString attr]

applyOp :: String -> [Value] -> EvalState Value
applyOp op args = getOp op >>= \f -> f args

evalBinOp :: BinOp -> Value -> Value -> EvalState Value
evalBinOp Add     x            y            = applyOp "+" [x, y]
evalBinOp Sub     x            y            = applyOp "-" [x, y]
evalBinOp Mult    x            y            = applyOp "*" [x, y]
evalBinOp Pow     x            y            = applyOp "^" [x, y]
evalBinOp And     x            y            = applyOp "&&" [x, y]
evalBinOp Or      x            y            = applyOp "||" [x, y]
evalBinOp Div     x            y            = applyOp "/" [x, y]
evalBinOp Lower   x            y            = applyOp "<" [x, y]
evalBinOp Greater x            y            = applyOp ">" [x, y]
evalBinOp Concat  x            y            = applyOp "++" [x, y]
evalBinOp Eq      x            y            = applyOp "==" [x, y]
evalBinOp NotEq   x            y            = applyOp "!=" [x, y]

evalBinOp At      (VList list) (VInt index) = return (list !! fromInteger index)

evalUOp :: UnaryOp -> Value -> EvalState Value
evalUOp Not    x = applyOp "!" [x]
evalUOp Negate x = applyOp "-" [x]


evalLiteral :: Lit -> EvalState Value
evalLiteral (Number  x) = pure $ VInt x
evalLiteral (Float   x) = pure $ VFloat x
evalLiteral (Boolean x) = pure $ VBool x
evalLiteral (Str     x) = pure $ VString $ T.unpack x
evalLiteral (Char'   x) = pure $ VChar x
evalLiteral (Array   x) = traverse (\(aPos :< aExpr) -> evalExpr' aExpr aPos) x
    >>= \v -> pure $ VList v
evalLiteral (Object x) =
    traverse (\(aPos :< aExpr) -> evalExpr' aExpr aPos) x
        >>= \v -> pure $ VObject v
evalLiteral Void = pure VoidV


