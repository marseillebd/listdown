module Config
  ( Config(..)
  , getConfig
  ) where

import Options.Applicative

data Config = Config
  { port :: Int
  , usersRoot :: FilePath
  , staticDir :: FilePath
  }

config :: Parser Config
config = Config
  <$> option auto
      ( long "port"
     <> metavar "PORT"
     <> help "Port to run the server on" )
  <*> strOption
      ( long "users-dir"
     <> metavar "DIRECTORY"
     <> help "Path to directory that holds the user directories" )
  <*> strOption
      ( long "static-dir"
     <> metavar "DIRECTORY"
     <> help "Path to directory that holds static assets" )

getConfig :: IO Config
getConfig = execParser $ info
  (config <**> helper)
   ( fullDesc
  <> progDesc "Run a Listdown server"
  <> header "listdown - a web app to store and manipulate hierarchical, hyperlinked notes" )