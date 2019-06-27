{-# LANGUAGE LambdaCase #-}

module Language.Interpreter.ProgramEval
    ( evalProgram
    )
where

import           Language.Parser.AST     hiding ( Fn )
import           Language.Interpreter.ExprEval
import           Language.Interpreter.DefaultEnv
import           Language.Interpreter.Types
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                , runExceptT
                                                )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as M
import           System.Directory               ( doesFileExist )
import           Language.Parser.ProgramParser
import           Data.Text                      ( pack )
import           Text.Megaparsec                ( parse
                                                , eof
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Comonad.Cofree
import           Text.Megaparsec.Pos            ( mkPos )
import qualified Control.Monad.State           as S
import           Control.Applicative            ( (<|>) )



data EvalContext = TopLevelCtx | InFnCtx

existsInSameScope :: String -> EvalState Bool
existsInSameScope name = S.gets (exists name . head)

insertInScope :: String -> Value -> EvalState ()
insertInScope name t = S.modify (\(s : ss) -> insert name t s : ss)

enterScope :: EvalState ()
enterScope = S.modify ((:) (M.fromList []))

leaveScope :: EvalState ()
leaveScope = S.modify tail

lookupScope :: String -> SourceSpan -> EvalState Value
lookupScope id pos =
    S.gets lookup' >>= maybe (throwError $ Unbound (begin pos) id) pure
  where
    lookup' :: [Scope Value] -> Maybe Value
    lookup' = foldr (<|>) Nothing . map (get id)

evalProgram :: Program -> EvalState Value
evalProgram (Program []   ) = pure VoidV
evalProgram (Program stmts) = evalStatements stmts TopLevelCtx

evalStatements :: [Statement] -> EvalContext -> EvalState Value
evalStatements [] _ = pure VoidV
evalStatements (ReturnStmt _ : _) TopLevelCtx =
    throwError $ Custom undefined "Illegal return statement"
evalStatements (ReturnStmt expr : _    ) InFnCtx = evalExpr expr
evalStatements (stmt            : stmts) ctx     = evalStmt stmt ctx >>= \case
    VoidV -> evalStatements stmts ctx
    value -> pure value

evalStmt :: Statement -> EvalContext -> EvalState Value
evalStmt (ReturnStmt _) TopLevelCtx =
    throwError $ Custom undefined "Illegal return statement"
evalStmt (VarDecl name _ expr) _ = do
    v <- evalExpr expr
    insertInScope name v
    pure VoidV
evalStmt (FnDecl name paramsT stmts) _ =
    let params = map (\(Param s _) -> s) paramsT
        fn     = Fn (\args -> invokeFn stmts args params)
    in  insertInScope name fn *> pure VoidV
evalStmt (CallStmt name params@((pos :< _) : _)) _ =
    lookupScope name pos >>= \case
        Fn fn -> do
            args <- mapM (\expr -> evalExpr expr) params
            fn args
evalStmt (IfStmt expr stmts elseBlock) ctx = do
    VBool isTrue <- evalExpr expr
    case (isTrue, elseBlock) of
        (True , _          ) -> evalStatements stmts ctx
        (False, Just stmts') -> evalStatements stmts' ctx
        (False, Nothing    ) -> pure VoidV
evalStmt (ReturnStmt expr   ) InFnCtx = evalExpr expr
evalStmt (Assign name expr _) _       = do
    v <- evalExpr expr
    insertInScope name v
    pure VoidV

invokeFn :: [Statement] -> [Value] -> [String] -> EvalState Value
invokeFn stmts given expected = do
    if length given == length expected
        then do
            enterScope
            mapM_ (\(name, value) -> insertInScope name value)
                  (zip expected given)
            evalStatements stmts InFnCtx <* leaveScope
        else
            throwError
            $  Custom undefined
            $  "Expected "
            <> show (length expected)
            <> " arguments but "
            <> show (length given)
            <> " were given"
