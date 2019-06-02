{-# LANGUAGE LambdaCase #-}

module Language.Interpreter
    ( eval
    )
where

import           Language.Parser.ProgramParser
import           Language.Interpreter.ProgramEval
import           Language.PrettyPrinter
import           Text.Megaparsec
import           Language.Interpreter.DefaultEnv
import qualified Data.Text                     as T
import           Control.Monad.Except           ( runExceptT )

eval :: String -> IO ()
eval buffer = return (parse (parseProgram <* eof) "" (T.pack buffer)) >>= \case
    Left  e   -> red $ errorBundlePretty $ e
    Right ast -> do
        (runExceptT $ evalProgram ast defaultEnv) >>= \case
            Left  e -> red $ show e
            Right _ -> return ()

