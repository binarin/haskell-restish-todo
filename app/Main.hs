module Main where

import Control.Exception (throw)
import Config (AppConfig, Host, Port, ProcessEnvironment(..), makeAppConfig, CompleteAppConfig)
import Options.Applicative (help, metavar, short, long, strOption, optional, progDesc, CommandFields, Mod, Parser, ParserInfo, subparser, execParser, info, argument, str, idm, command)
import System.Environment (getEnvironment)
import Text.Pretty.Simple (pPrint)

-- import Lib
-- import Control.Monad (join)

data Options = Options
  { cfgPath :: Maybe FilePath
  , cmd :: Command
  }

data Command = Serve
             | ShowConfig
             deriving (Eq)


parseCommands :: Parser Command
parseCommands = subparser commands
  where
    serverCmd :: ParserInfo Command
    serverCmd = info (pure Serve) (progDesc "Start the server")

    showCmd :: ParserInfo Command
    showCmd = info (pure ShowConfig) (progDesc "Show configuration")

    commands :: Mod CommandFields Command
    commands = command "server" serverCmd
      <> command "show-config" showCmd

parseOptions :: Parser (Maybe FilePath)
parseOptions = optional $ strOption ( long "config"
                                    <> short 'c'
                                    <> metavar "FILENAME"
                                    <> help "Configuration file (.json/.toml)"
                                    )


parseCmdLine :: Parser Options
parseCmdLine = Options <$> parseOptions <*> parseCommands

pullEnvironment :: IO ProcessEnvironment
pullEnvironment = ProcessEnvironment <$> getEnvironment

runServer :: CompleteAppConfig -> IO ()
runServer _ = server

-- | Start up server and serve requests
server :: IO ()
server = putStrLn "<SERVER START>"

main :: IO ()
main = do
  opts <- execParser (info parseCmdLine idm)
  config <- mkConfig opts
  case cmd opts of
    Serve -> runServer config
    ShowConfig -> pPrint config
  where
    mkConfig :: Options -> IO CompleteAppConfig
    mkConfig (Options cfgPath cmd) = do
      env <- ProcessEnvironment <$> getEnvironment
      makeAppConfig env cfgPath >>= \case
        Left e -> throw e
        Right cf -> pure cf
