
module Language.Cli.Opt
    ( runCli
    )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Language.Parser

data Opt = Opt
    { file :: Maybe String }

opt :: Parser Opt
opt =
    Opt
        <$> ( optional
            $ argument str (metavar "TARGET" <> help "File to execute")
            )

runCli :: IO ()
runCli = getOpt =<< execParser headerInfo

getOpt :: Opt -> IO ()
getOpt (Opt Nothing    ) = putStrLn "Interactive mode"
getOpt (Opt (Just file)) = parseFile file

headerInfo :: ParserInfo Opt
headerInfo = info
    (opt <**> helper)
    (fullDesc <> progDesc "Iris lang cli" <> header
        "iris - Command line utility"
    )
