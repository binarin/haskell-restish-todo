module Config where

import System.Environment (getEnvironment)
import Control.Exception (try, throw, Exception)
import Control.Applicative
import Control.Monad (join)
import Data.Functor.Identity
import Data.Default
import Data.Aeson (eitherDecode, FromJSON(..), toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import GHC.Generics
import Data.Bifunctor (first, bimap)
import qualified Data.ByteString.Lazy as BL
import Text.Toml (parseTomlDoc)
import Data.Text.IO as DTI
import Text.Read (readMaybe)
import Data.Maybe
import qualified System.FilePath as FP

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
  def = AppConfig Nothing Nothing

data ConfigurationError = ConfigParseError String | InvalidPath FP.FilePath
  deriving (Show)

instance Exception ConfigurationError

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
       Right table -> case parseEither parseJSON (toJSON table) of
                        Left err -> pure $ Left $ ConfigParseError err
                        Right it -> pure $ Right it

newtype ProcessEnvironment = ProcessEnvironment { getProcessEnv :: [(String, String)] } deriving (Eq)

class FromENV cfg where
  fromENV :: ProcessEnvironment -> IO (Either ConfigurationError cfg)

instance FromENV PartialAppConfig where
  fromENV (ProcessEnvironment env) = pure $ Right $ AppConfig { host = host, port = port }
    where
      host :: Maybe Host
      host = lookup "TODO_HOST" env

      port :: Maybe Port
      port = readMaybe =<< lookup "TODO_PORT" env

class MergeOverridable a where
  mergeOverride :: a -> a -> a

instance MergeOverridable PartialAppConfig where
  mergeOverride a b = AppConfig { host = resolveMaybes host, port = resolveMaybes port }
    where
      resolveMaybes :: (PartialAppConfig -> Maybe x) -> Maybe x
      resolveMaybes fn = fn a <|> fn b

mergeInPartial :: CompleteAppConfig -> PartialAppConfig -> CompleteAppConfig
mergeInPartial c p = AppConfig { host = maybe (host c) Identity (host p)
                               , port = maybe (port c) Identity (port p)
                               }

rightOrThrow :: (Exception a) => Either a b -> IO b
rightOrThrow (Left e) = throw e
rightOrThrow (Right e) = pure e

buildConfigWithDefault :: CompleteAppConfig -> [PartialAppConfig] -> CompleteAppConfig
buildConfigWithDefault orig partials = orig `mergeInPartial` combinedPartials
  where
    combinedPartials :: PartialAppConfig
    combinedPartials = foldl mergeOverride def partials

makeAppConfig :: ProcessEnvironment -> Maybe FP.FilePath -> IO (Either ConfigurationError CompleteAppConfig)
makeAppConfig env configPath = try generateConfig
  where
    fileResult :: FP.FilePath -> IO (Either ConfigurationError PartialAppConfig)
    fileResult path = case FP.takeExtension path of
      ".toml" -> fromTOMLFile path
      ".json" -> fromJSONFile path
      ext -> pure $ Left $ InvalidPath ext

    envResult :: IO (Either ConfigurationError PartialAppConfig)
    envResult = fromENV env

    generateConfig :: IO CompleteAppConfig
    generateConfig = do
      fileCfg <- case configPath of
        Nothing -> pure []
        Just path -> do
          res <- fileResult path >>= rightOrThrow
          pure [res]
      envCfg <- envResult >>= rightOrThrow
      let partials = envCfg : fileCfg
      pure $ buildConfigWithDefault def partials
