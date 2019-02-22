module Config where

import Data.Functor.Identity
import Data.Default
import Data.Aeson (eitherDecode, FromJSON(..), toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import GHC.Generics
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Text.Toml (parseTomlDoc)
import Data.Text.IO as DTI

type Host = String
type Port = Integer

data AppConfig f = AppConfig
  { host :: f Host
  , port :: f Port
  }

type CompleteAppConfig = AppConfig Identity
deriving instance Generic CompleteAppConfig
deriving instance Eq CompleteAppConfig
deriving instance Show CompleteAppConfig
deriving instance FromJSON CompleteAppConfig

type PartialAppConfig = AppConfig Maybe
deriving instance Generic PartialAppConfig
deriving instance Eq PartialAppConfig
deriving instance Show PartialAppConfig
deriving instance FromJSON PartialAppConfig

defaultHost :: Host
defaultHost = "localhost"

defaultPort :: Port
defaultPort = 5000

instance Default (AppConfig Identity) where
  def = AppConfig (Identity defaultHost) (Identity defaultPort)

instance Default (AppConfig Maybe) where
  def = AppConfig (Just defaultHost) (Just defaultPort)

data ConfigurationError = ConfigParseError String
  deriving (Show)

class (FromJSON cfg) => FromJSONFile cfg where
  fromJSONFile :: FilePath -> IO (Either ConfigurationError cfg)

instance FromJSONFile PartialAppConfig where
  fromJSONFile path = decodeAndTransform <$> BL.readFile path
    where
      decodeAndTransform :: BL.ByteString -> Either ConfigurationError PartialAppConfig
      decodeAndTransform = first ConfigParseError . eitherDecode

class (FromJSONFile cfg) => FromTOMLFile cfg where
  fromTOMLFile :: FilePath -> IO (Either ConfigurationError cfg)

instance FromTOMLFile PartialAppConfig where
   fromTOMLFile path = do
     text <- DTI.readFile path
     case parseTomlDoc ("parsing " ++ path) text of
       Left e -> pure $ Left $ ConfigParseError $ show e
       Right table -> do
         pure $ first ConfigParseError $ eitherDecode $ Aeson.encode table
