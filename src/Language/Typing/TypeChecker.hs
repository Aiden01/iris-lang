{-# LANGUAGE LambdaCase, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Language.Typing.TypeChecker
  ( runTypeChecking
  )
where

import           Prelude                 hiding ( read )
import           Language.Parser.AST
import           Control.Lens            hiding ( (:<) )
import           Language.Typing.Types
import           Control.Comonad                ( extract )
import           Control.Monad.Reader
import           Language.Interpreter.Types     ( exists
                                                , get
                                                , insert
                                                )
import           Data.Maybe                     ( isJust )
import           Control.Monad.Except           ( throwError
                                                , runExceptT
                                                )
import           Control.Comonad.Cofree
import qualified Data.Map                      as M


compareTypes :: SourceSpan -> Type -> Type -> TypeCheck Type
compareTypes pos t0 t1 | t0 == t1  = pure t0
                       | otherwise = throwError $ Mismatch t0 t1 pos
read = asks . view


getVar :: String -> SourceSpan -> TypeCheck Type
getVar name pos = do
  env <- read tcEnv
  case get name env of
    Just t  -> pure t
    Nothing -> throwError $ NotInScope name pos

instance TypeCheckable Lit Type where
  tc (Number _) = pure TInt
  tc (Boolean _) = pure TBool
  tc (Str _) = pure TString
  tc (Array (e:exprs)) = do
    expected <- tc e
    traverse (\ e -> tc e >>= compareTypes (extract e) expected) exprs
    pure $ TArray expected
  tc (Char' _) = pure TChar
  tc (Float _) = pure TFloat
  tc Void = pure VoidT

instance TypeCheckable a Type => TypeCheckable [a] [Type] where
  tc = traverse tc

instance TypeCheckable Expr Type where
  tc (_ :< Literal lit) = tc lit
  tc (pos :< Var name) = getVar name pos
  tc (pos :< CallExpr expr exprs) = tc expr >>= \case
    Fn params t0 ->
      if length exprs == length params then do
        args <- tc exprs
        mapM_ (uncurry (compareTypes pos)) $ zip params args
        pure t0
      else
        throwError $ Custom ("Function takes " <> show (length params) <> " arguments, but " <> show (length exprs) <> " were passed") pos
    _ -> throwError $ Custom "Not a function" pos
  tc (pos :< Range start stop) = do
    t0 <- tc start
    t1 <- tc stop
    compareTypes pos t0 t1
    case t0 of
      TChar -> pure $ TArray TChar
      TInt -> pure $ TArray TInt
      _ -> throwError $ Custom ("Could not create a range from type " <> show t0) pos

instance TypeCheckable Param Type where
  tc (Param _ t) = pure t

tcStatements :: [Statement] -> TypeCheck ()
tcStatements stmts = foldM_ (\f i -> (. f) <$> local f (tc i)) id stmts

checkRedecl :: String -> SourceSpan -> TypeCheck ()
checkRedecl name pos = asks (exists name . view tcEnv) >>= \case
  True  -> throwError $ Redeclaration name pos
  False -> pure ()

enterFunction
  :: String -> [Param] -> [Type] -> Type -> TypeCheckEnv -> TypeCheckEnv
enterFunction name params paramTypes t =
  let paramsMap = M.fromList $ map (\(Param n t) -> (n, t)) params
      fnEnv     = M.insert name (Fn paramTypes t) paramsMap
  in  (tcExpected .~ Just t) . (tcEnv %~ M.union fnEnv)

instance TypeCheckable Statement (TypeCheckEnv -> TypeCheckEnv) where
  tc (ReturnStmt expr) = read tcExpected >>= \case
    Nothing -> throwError $ Custom "Illegal return statement" (extract expr)
    Just expected -> do
      t <- tc expr
      compareTypes (extract expr) expected t
      pure id

  tc (WhileStmt expr stmts) = tc expr >>= \case
    TBool -> tcStatements stmts *> pure id
    t -> throwError $ Mismatch TBool t (extract expr)
  tc (VarDecl name maybeType expr) = do
    checkRedecl name $ extract expr
    t0 <- case maybeType of
            Just t -> tc expr >>= compareTypes (extract expr) t
            Nothing -> tc expr
    pure (tcEnv %~ insert name t0)
  tc (FnDecl name params stmts t) = do
    checkRedecl name undefined
    types <- tc params
    local (enterFunction name params types t) $ tcStatements stmts
    pure (tcEnv %~ insert name (Fn types t))
  tc (Assign name expr _) = do
    t0 <- getVar name $ extract expr
    t1 <- tc expr
    compareTypes (extract expr) t0 t1
    pure id
  tc (IfStmt cond stmts elseBlock) = do
    t <- tc cond
    compareTypes (extract cond) TBool t
    tcStatements stmts
    case elseBlock of
      Just stmts' -> tcStatements stmts' *> pure id
      Nothing -> pure id

instance TypeCheckable Program () where
  tc (Program stmts) = tcStatements stmts

defaultTypeCheckEnv :: TypeCheckEnv
defaultTypeCheckEnv =
  TypeCheckEnv {_tcExpected = Nothing, _tcEnv = M.fromList []}

runTypeChecking :: Program -> IO (Either TypeError ())
runTypeChecking p = runExceptT $ runReaderT (tc p) defaultTypeCheckEnv
