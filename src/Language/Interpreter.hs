{-# LANGUAGE LambdaCase #-}

module Language.Interpreter
    ( eval
    )
where

import           Language.Parser.ProgramParser
import           Language.Parser.AST
import           Language.Interpreter.ProgramEval
import           Language.Interpreter.Types
import           Language.Parser.ExprParser
import           Language.Interpreter.ExprEval
import           Language.Typing.TypeChecker
import           Language.PrettyPrinter
import           Text.Megaparsec
import           Language.Interpreter.DefaultEnv
import qualified Data.Text                     as T
import           Control.Monad.Except           ( runExceptT )
import           Data.Void                      ( Void )
import           Data.Map                       ( union
                                                , fromList
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.State            ( evalStateT )

-- getPrelude :: EvalState Value
-- getPrelude = do
--     Right ast <- parse (parseProgram <* eof) "" . T.pack <$> readFile
--         "lib/prelude.iris"

--     evalProgram ast

eval :: String -> String -> IO ()
eval fileName buffer =
    return (parse (parseProgram <* eof) fileName (T.pack buffer)) >>= \case
        Left  e   -> red $ errorBundlePretty $ e
        Right ast -> do
            (runExceptT $ evalStateT (evalProgram ast) [defaultEnv]) >>= \case
                Left  e -> red $ show e
                Right _ -> return ()


-- eval' :: String -> IO ()
-- eval' buffer = return (parse (parseExpr <* eof) "" (T.pack buffer)) >>= \case
--     Left  e   -> red $ errorBundlePretty $ e
--     Right ast -> do
--         (runExceptT $ evalExpr ast defaultEnv) >>= \case
--             Left  e -> red $ show e
--             Right r -> print r
