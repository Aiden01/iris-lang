{-# LANGUAGE LambdaCase #-}

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
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as M

type EnvResult = ExceptT VError IO (Scope Value)

evalProgram :: Program -> Scope Value -> EnvResult
evalProgram (Program []   ) env = return env
evalProgram (Program stmts) env = evalStatements stmts env

evalStatements :: [Statement] -> Scope Value -> EnvResult
evalStatements [] env = return env
evalStatements stmts env =
    foldl (\env' stmt -> evalStmt stmt =<< env') (return env) stmts

evalForStmt :: String -> [Value] -> [Statement] -> Scope Value -> EnvResult
evalForStmt _ [] _ env = return env
evalForStmt id (v : vs) stmts env =
    let newEnv = insert id v env
    in  evalStatements stmts newEnv >>= evalForStmt id vs stmts

evalFnStatements :: [Statement] -> Scope Value -> VResult
evalFnStatements []                      _   = return VoidV
evalFnStatements ((ReturnStmt expr) : _) env = evalExpr expr env
evalFnStatements (stmt : stmts) env =
    evalFnStatements stmts =<< evalStmt stmt env

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
evalStmt (ForStmt id expr block) env = do
    list <- evalExpr expr env
    case list of
        VList v -> evalForStmt id v block env
        _       -> throwError $ Custom "Mismatched types"
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
evalStmt (FnDecl name paramsT p) env =
    let params = map (\(Param s _) -> s) paramsT
        block  = fromMaybe [] p
        fn     = Fn (\args -> invokeFn block env args params)
    in  return $ insert name fn env
  where
    invokeFn :: [Statement] -> Scope Value -> [Value] -> [String] -> VResult
    invokeFn stmts env given expected = if length given == length expected
        then do
            let fnEnv = foldr (\(name, value) env -> insert name value env)
                              (M.fromList [])
                              (zip expected given)
            evalFnStatements stmts (M.union fnEnv env)
        else
            throwError
            $  Custom
            $  "Expected "
            <> show (length expected)
            <> " arguments but "
            <> show (length given)
            <> " were given"
