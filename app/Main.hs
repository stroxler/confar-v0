module Main where

import Options.Applicative (
  Parser, ParserInfo, execParser,
  (<**>),
  info, helper,
  fullDesc, progDesc, header,
  strOption, long, short, metavar, help,
  )
import Shelly (
  shelly, verbosely, fromText,
  )
import Data.Text (
  pack
  )
import Lib

data ConfarOpts = ConfarOpts { source :: String
                             , target :: String }


parseConfarOpts :: IO ConfarOpts
parseConfarOpts = execParser $
  info (parser <**> helper) ( fullDesc
                              <> progDesc "Run confar to set up repo symlinks"
                              <> header "confar - config as repositories"
                            )
  where
    parser :: Parser ConfarOpts
    parser = ConfarOpts
      <$> strOption ( long "source"
                      <> short 's'
                      <> metavar "PATH"
                      <> help "source directory (repo)"
                    )
      <*> strOption ( long "dest"
                      <> short 'd'
                      <> metavar "PATH"
                      <> help "target directory"
                    )

main :: IO ()
main = do
  opts <- parseConfarOpts
  let
    src = fromText $ pack $ source opts
    tgt = fromText $ pack $ target opts
  shelly $ verbosely $ confarInstall src tgt
