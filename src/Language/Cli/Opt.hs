
module Language.Cli.Opt
    ( runCli
    )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Language.Parser
import           Control.Monad                  ( forever )
import           System.IO
import           Language.Interpreter


data Opt = Opt
    { file :: Maybe String }

opt :: Parser Opt
opt =
    Opt <$> (optional $ argument str (metavar "FILE" <> help "File to execute"))

runCli :: IO ()
runCli = getOpt =<< execParser headerInfo

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

getOpt :: Opt -> IO ()
-- getOpt (Opt Nothing) = putStrLn "Iris REPL - Version 1.0.0" >>= \_ ->
--     forever $ do
--         stdin <- prompt "λ> "
--         eval' stdin
getOpt (Opt (Just file)) = readFile file >>= eval file

headerInfo :: ParserInfo Opt
headerInfo = info
    (opt <**> helper)
    (fullDesc <> progDesc "Iris lang cli" <> header
        "iris - Command line utility"
    )
