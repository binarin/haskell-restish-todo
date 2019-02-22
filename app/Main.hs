module Main where

import Lib
import Data.Semigroup((<>))
import Options.Applicative (CommandFields, Mod, Parser, ParserInfo, subparser, execParser, info, argument, str, idm, command)
import Control.Monad (join)

newtype Options = Options { cmd :: Command }
data Command = Serve Host Port

-- | Start up server and serve requests
server :: IO ()
server = putStrLn "<SERVER START>"

-- subparser :: Mod CommandFields a -> Parser a

opts :: Parser (IO ())
opts = subparser commands
  where
    serverAction :: Parser (IO ())
    serverAction = pure server

    serverCmd :: ParserInfo (IO ())
    serverCmd = info serverAction idm

    commands :: Mod CommandFields (IO ())
    commands = command "server" serverCmd


main :: IO ()
main = join $ execParser parser
  where
    parser :: ParserInfo (IO ())
    parser = info opts idm
