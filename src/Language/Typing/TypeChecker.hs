{-# LANGUAGE LambdaCase #-}
module Language.Typing.TypeChecker
    ( tc
    )
where

import           Language.Typing.Types
import           Language.Parser.AST            ( Lit(..)
                                                , Expr
                                                , UnaryOp(..)
                                                , Statement(..)
                                                , Program(..)
                                                , Param(..)
                                                , BinOp(..)
                                                , ExprF(..)
                                                , Type(..)
                                                , SourceSpan(..)
                                                , StmtContext(..)
                                                )
import           Control.Comonad.Cofree
import qualified Language.Interpreter.Types    as T
import           Control.Monad.State            ( gets
                                                , modify
                                                , evalStateT
                                                )
import           Control.Monad.Except           ( throwError
                                                , runExceptT
                                                )
import           Data.Map                       ( fromList )
import           Language.PrettyPrinter
import           Language.Typing.DefaultEnvType
import           Control.Applicative            ( (<|>) )
import           Data.Maybe                     ( fromMaybe )

exprs2Types :: [Expr] -> TcState [Type]
exprs2Types = traverse tcExprType

compareTypes :: [Type] -> [Type] -> SourceSpan -> TcState ()
compareTypes expected given pos =
    mapM_
            (\(t0, t1) ->
                if t0 == t1 then pure () else throwError $ Mismatch t0 t1 pos
            )
        $ zip expected given

enterScope :: TcState ()
enterScope = modify ((:) (fromList []))

leaveScope :: TcState ()
leaveScope = modify tail

insertInScope :: String -> Type -> TcState ()
insertInScope name t = modify (\(s : ss) -> T.insert name t s : ss)

existsInSameScope :: String -> TcState Bool
existsInSameScope name = gets (T.exists name . head)

tcLitType :: Lit -> SourceSpan -> TcState Type
tcLitType (Number  _       ) _   = pure TInt
tcLitType (Boolean _       ) _   = pure TBool
tcLitType (Str     _       ) _   = pure TString
tcLitType (Array   (e : ex)) pos = do
    expected      <- tcExprType e
    isHomogeneous <- all (== expected) <$> exprs2Types ex
    case isHomogeneous of
        True  -> pure $ TArray expected
        False -> throwError $ Custom "Expected array of homogeneous types" pos
tcLitType (Char' _) _ = pure TChar
tcLitType (Float _) _ = pure TFloat
tcLitType Void      _ = pure VoidT

tcBinOpType :: BinOp -> Expr -> Expr -> TcState Type
tcBinOpType op e@(pos :< _) e' = do
    t0 <- tcExprType e
    t1 <- tcExprType e'
    case (op, t0, t1) of
        (Add, TInt  , TInt  ) -> pure TInt
        (Add, TFloat, TFloat) -> pure TFloat
        (Add, TFloat, TInt  ) -> pure TFloat
        (Add, TInt  , TFloat) -> pure TFloat
        (Add, TInt  , b     ) -> throwError $ Mismatch TInt b pos
        (Add, a     , TInt  ) -> throwError $ Mismatch TInt a pos
        (Add, TFloat, b     ) -> throwError $ Mismatch TFloat b pos
        (Add, a     , TFloat) -> throwError $ Mismatch TFloat a pos

tcExprType :: Expr -> TcState Type
tcExprType (pos  :< Literal lit                     ) = tcLitType lit pos
tcExprType (pos  :< Var     name                    ) = lookupScope name pos
tcExprType (_    :< BinOp op e e'                   ) = tcBinOpType op e e'
tcExprType (pos0 :< CallExpr expr@(pos1 :< _) params) = do
    given <- exprs2Types params
    fnT   <- tcExprType expr
    case fnT of
        Fn expected returnT -> compareTypes given expected pos1 *> pure returnT
        _                   -> throwError $ Custom "not a function" pos0

tcStmtsType :: [Statement] -> TcState ()
tcStmtsType []             = pure ()
tcStmtsType (stmt : stmts) = tcStmtType stmt *> tcStmtsType stmts

extractParamTypes :: [Param] -> [Type]
extractParamTypes = map (\(Param _ t) -> t)

insertFnType :: String -> [Param] -> [TypeEnv] -> [TypeEnv]
insertFnType name params (s : ss) =
    (T.insert name (Fn (extractParamTypes params) TInt) s) : ss

lookupScope :: String -> SourceSpan -> TcState Type
lookupScope id pos =
    gets lookup' >>= maybe (throwError $ NotInScope id pos) pure
  where
    lookup' :: [TypeEnv] -> Maybe Type
    lookup' = foldr (<|>) Nothing . map (T.get id)

lookupImmediateScope :: String -> SourceSpan -> TcState Type
lookupImmediateScope id pos =
    gets (T.get id . head) >>= maybe (throwError $ NotInScope id pos) pure

modifyScope :: String -> Type -> SourceSpan -> [TypeEnv] -> [TypeEnv]
modifyScope id t pos (s : ss) =
    if T.exists id s then T.modify id t s : ss else modifyScope id t pos ss

tcStmtType :: Statement -> TcState ()
tcStmtType (VarDecl name (Just t0) expr@(pos :< _)) =
    existsInSameScope name >>= \case
        True  -> throwError $ Redeclaration name pos
        False -> do
            t1 <- tcExprType expr
            if t0 == t1
                then insertInScope name t0
                else throwError $ Mismatch t0 t1 pos
tcStmtType (Assign name expr@(pos :< _) ctx) = do
    t0 <- case ctx of
        InFunction -> lookupImmediateScope name pos
        TopLevel   -> lookupScope name pos
    t1 <- tcExprType expr
    if t0 == t1
        then modify (modifyScope name t1 pos)
        else throwError $ Mismatch t0 t1 pos
tcStmtType (FnDecl name params stmts) = existsInSameScope name >>= \case
    True  -> throwError $ Redeclaration name undefined
    False -> enterScope *> tcStmtsType stmts *> leaveScope *> modify
        (insertFnType name params)
tcStmtType (CallStmt name params@((pos :< _) : _)) =
    lookupScope name pos >>= \case
        Fn types _ -> do
            given <- exprs2Types params
            compareTypes given types pos
            pure ()
        _ -> throwError $ Custom (name <> " is not a function") pos
tcStmtType (WhileStmt expr@(pos :< _) stmts) = tcExprType expr >>= \case
    TBool -> tcStmtsType stmts
    t     -> throwError $ Mismatch TBool t pos

tcProgram :: Program -> TcState ()
tcProgram (Program stmts) = tcStmtsType stmts

tc :: Program -> IO ()
tc p = (runExceptT $ evalStateT (tcProgram p) [defaultEnvType]) >>= \case
    Left  e -> red $ show e
    Right _ -> green "Type checked!"
