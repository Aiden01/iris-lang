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
                                                , Pattern(..)
                                                , AnPattern
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
import           Data.Map                       ( fromList
                                                , toList
                                                )
import           Language.PrettyPrinter
import           Language.Typing.DefaultEnvType
import           Control.Applicative            ( (<|>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( find )


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

tcPatternType :: AnPattern -> TcState Type
tcPatternType (pos :< PLit lit) = tcLitType lit pos

tcBranch :: Type -> Type -> (AnPattern, Expr) -> TcState ()
tcBranch expectedPT expectedExprT (p, expr@(pos :< _)) = do
  t0 <- tcPatternType p
  t1 <- tcExprType expr
  compareTypes [expectedPT, expectedExprT] [t0, t1] pos

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
tcExprType (pos :< Match expr ((p, e) : branches)) = do
  expectedPT <- tcExprType expr
  t0         <- tcPatternType p
  compareTypes [expectedPT] [t0] pos
  expectedExprT <- tcExprType e
  traverse (tcBranch expectedPT expectedExprT) branches
  pure expectedExprT
tcStmtsType :: [Statement] -> Bool -> TcState ()
tcStmtsType [] _ = pure ()
tcStmtsType (stmt : stmts) inFn =
  tcStmtType stmt inFn *> tcStmtsType stmts inFn

extractParamTypes :: [Param] -> [Type]
extractParamTypes = map (\(Param _ t) -> t)

insertFnParams :: [Param] -> TcState ()
insertFnParams params = modify
  (\(s : ss) ->
    fromList (foldl (\env (Param id t0) -> (id, t0) : env) (toList s) params)
      : ss
  )

insertFnType :: String -> [Param] -> Type -> TcState ()
insertFnType name params t =
  modify (\(s : ss) -> T.insert name (Fn (extractParamTypes params) t) s : ss)




lookupScope :: String -> SourceSpan -> TcState Type
lookupScope id pos =
  gets lookup' >>= maybe (throwError $ NotInScope id pos) pure
 where
  lookup' :: [TypeEnv] -> Maybe Type
  lookup' = foldr (<|>) Nothing . map (T.get id)

lookupImmediateScope :: String -> SourceSpan -> TcState Type
lookupImmediateScope id pos =
  gets (T.get id . head) >>= maybe (throwError $ NotInScope id pos) pure



getFnReturnType :: Bool -> SourceSpan -> TcState Type
getFnReturnType inFn pos
  | not inFn = throwError $ Custom "Illegal return statement" pos
  | otherwise = gets getFnReturnType' >>= \case
    Nothing -> throwError $ Custom "Illegal return statement" pos
    Just x  -> pure x
 where
  isFn (Fn _ _) = True
  isFn _        = False
  getFnReturnType' :: [TypeEnv] -> Maybe Type
  getFnReturnType' []       = Nothing
  getFnReturnType' (s : ss) = case find (isFn . snd) $ toList s of
    Just (_, Fn _ x) -> Just x
    Nothing          -> getFnReturnType' ss



modifyScope :: String -> Type -> SourceSpan -> [TypeEnv] -> [TypeEnv]
modifyScope id t pos (s : ss) =
  if T.exists id s then T.modify id t s : ss else modifyScope id t pos ss

tcStmtType :: Statement -> Bool -> TcState ()
tcStmtType (VarDecl name maybeT expr@(pos :< _)) _ =
  existsInSameScope name >>= \case
    True  -> throwError $ Redeclaration name pos
    False -> do
      t1 <- tcExprType expr
      let t0 = fromMaybe t1 maybeT
      if t0 == t1
        then insertInScope name t0
        else throwError $ Mismatch t0 t1 pos
tcStmtType (ReturnStmt expr@(pos :< _)) inFn = do
  t0 <- getFnReturnType inFn pos
  t1 <- tcExprType expr
  if t0 == t1 then pure () else throwError $ Mismatch t0 t1 pos
tcStmtType (Assign name expr@(pos :< _) ctx) _ = do
  t0 <- case ctx of
    InFunction -> lookupImmediateScope name pos
    TopLevel   -> lookupScope name pos
  t1 <- tcExprType expr
  if t0 == t1
    then modify (modifyScope name t1 pos)
    else throwError $ Mismatch t0 t1 pos
tcStmtType (FnDecl name params stmts t) _ = existsInSameScope name >>= \case
  True  -> throwError $ Redeclaration name undefined
  False -> do
    insertFnType name params t
    enterScope
    insertFnParams params
    tcStmtsType stmts True
    leaveScope
tcStmtType (CallStmt name params@((pos :< _) : _)) _ =
  lookupScope name pos >>= \case
    Fn types _ -> do
      given <- exprs2Types params
      compareTypes types given pos
      pure ()
    _ -> throwError $ Custom (name <> " is not a function") pos
tcStmtType (WhileStmt expr@(pos :< _) stmts) inFn = tcExprType expr >>= \case
  TBool -> tcStmtsType stmts inFn
  t     -> throwError $ Mismatch TBool t pos

tcProgram :: Program -> TcState ()
tcProgram (Program stmts) = tcStmtsType stmts False

tc :: Program -> IO ()
tc p = (runExceptT $ evalStateT (tcProgram p) [defaultEnvType]) >>= \case
  Left  e -> red $ show e
  Right _ -> green "Type checked!"
