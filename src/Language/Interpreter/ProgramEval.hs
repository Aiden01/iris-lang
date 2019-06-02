module Language.Interpreter.ProgramEval
    ( evalProgram
    )
where

import           Language.Parser.AST
import           Language.Interpreter.ExprEval
import           Language.Interpreter.DefaultEnv
import           Language.Interpreter.Types
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )

type EnvResult = ExceptT VError IO (Scope Value)

evalProgram :: Program -> Scope Value -> EnvResult
evalProgram (Program []   ) env = return env
evalProgram (Program stmts) env = evalStatements stmts env

evalStatements :: [Statement] -> Scope Value -> EnvResult
evalStatements []             env = return env
evalStatements (stmt : stmts) env = evalStatements stmts =<< evalStmt stmt env

evalStmt :: Statement -> Scope Value -> EnvResult
evalStmt (VarDecl name _ expr) env = do
    value <- evalExpr expr env
    case get name env :: Maybe Value of
        Nothing -> return $ insert name value env
        Just _  -> throwError $ Custom $ name <> " is already declared"
evalStmt (Assign name expr) env = do
    value <- evalExpr expr env
    case get name env :: Maybe Value of
        Nothing -> throwError $ Unbound name
        Just _  -> return $ modify name value env
evalStmt (CallStmt name params) env = do
    exprs <- traverse (flip evalExpr env) params
    case get name env of
        Nothing     -> throwError $ Custom $ "Undefined function " <> name
        Just (Fn f) -> f exprs >> return env
evalStmt (IfStmt cond block elseStmt) env = do
    expr <- makeTruthy <$> evalExpr cond env
    case (expr, block, elseStmt) of
        (True , stmts, _         ) -> evalStatements stmts env
        (False, _    , Just stmts) -> evalStatements stmts env
        (False, _    , Nothing   ) -> return env
evalStmt (WhileStmt cond p) env = do
    expr <- makeTruthy <$> evalExpr cond env
    let stmt = WhileStmt cond p
    case (expr, p) of
        (True , Just stmts) -> evalProgram stmts env >>= evalStmt stmt
        (True , Nothing   ) -> evalStmt stmt env
        (False, _         ) -> return env
