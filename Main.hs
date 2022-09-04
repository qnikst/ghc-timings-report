module Main where

import Options.Applicative

import qualified Options.Applicative as Options

import Control.Monad
import Compare
import Generate

main :: IO ()
main = join $ execParser (info (helper <*> opts) idm)
  where
    opts = subparser $
      command "generate" (info (helper <*> generate) (progDesc "Generate timings report"))
      <> command "compare" (info (helper <*> compareReports) (progDesc "Compare two reports"))

-- | Gather arguments for report generation.
generate :: Options.Parser (IO ())
generate = runGenerate
  <$> strOption
    ( short 'i'
      <> long "input"
      <> metavar "PROJECT_DIR"
      <> help "Haskell project directory"
    )

-- | Gather arguments for comparing reports.
compareReports :: Options.Parser (IO ())
compareReports = runCompare
  <$> strOption
    ( short 'b'
      <> long "before"
      <> metavar "REPORT_DIR"
      <> help "Directory with generated timings report from the previous build"
    )
  <*> strOption
    ( short 'a'
      <> long "after"
      <> metavar "REPORT_DIR"
      <> help "Directory with generated timings report from the last build"
    )
  <*> strOption
    ( short 'o'
      <> long "output"
      <> metavar "OUTPUT_DIR"
      <> help "Directory in which to store the output"
    )
