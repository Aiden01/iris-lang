{-# LANGUAGE LambdaCase #-}

module Language.Interpreter
    ( eval
    , eval'
    )
where

import           Language.Parser.ProgramParser
import           Language.Parser.AST
import           Language.Interpreter.ProgramEval
import           Language.Interpreter.Types
import           Language.Parser.ExprParser
import           Language.Interpreter.ExprEval
import           Language.PrettyPrinter
import           Text.Megaparsec
import           Language.Interpreter.DefaultEnv
import qualified Data.Text                     as T
import           Control.Monad.Except           ( runExceptT )
import           Data.Void                      ( Void )
import           Data.Map                       ( union )
import           Control.Monad.Trans            ( lift )

getPrelude :: EnvResult
getPrelude = do
    Right ast <-
        lift
            (parse (parseProgram <* eof) "" . T.pack <$> readFile
                "lib/prelude.iris"
            )
    evalProgram ast defaultEnv

eval :: String -> IO ()
eval buffer = return (parse (parseProgram <* eof) "" (T.pack buffer)) >>= \case
    Left  e   -> red $ errorBundlePretty $ e
    Right ast -> do
        Right prelude <- runExceptT getPrelude
        (runExceptT $ evalProgram ast prelude) >>= \case
            Left  e -> red $ show e
            Right _ -> return ()


eval' :: String -> IO ()
eval' buffer = return (parse (parseExpr <* eof) "" (T.pack buffer)) >>= \case
    Left  e   -> red $ errorBundlePretty $ e
    Right ast -> do
        (runExceptT $ evalExpr ast defaultEnv) >>= \case
            Left  e -> red $ show e
            Right r -> print r
