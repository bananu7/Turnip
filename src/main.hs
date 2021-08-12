module Main where

import Options.Applicative

import Turnip.Repl

replConfig :: Parser ReplConfig
replConfig = ReplConfig
      <$> strOption
          ( long "file"
         <> short 'f'
         <> metavar "PATH"
         <> value ""
         <> help "File to run" )
      <*> switch
          ( long "interactive"
         <> short 'i'
         <> help "Whether to be quiet" )

main :: IO ()
main = repl =<< execParser opts
  where
    opts = info (replConfig <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "Turnip REPL" )
