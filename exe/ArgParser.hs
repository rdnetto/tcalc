module ArgParser (parseArgs) where

import BasicPrelude
import Options.Applicative


parseArgs :: IO (Maybe FilePath)
parseArgs = execParser opts where
    opts = info (parser <**> helper) infoMod
    infoMod = concat [
            fullDesc,
            progDesc "Performs basic time-oriented computations.",
            header "tcalc"
        ]

parser :: Parser (Maybe FilePath)
parser = optional $ argument str (metavar "FILE")
