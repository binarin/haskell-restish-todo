{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import Data.List.NonEmpty(NonEmpty(..))
import Data.Semigroup (Last(..), Semigroup, sconcat)
import System.Environment (getEnvironment)
import Control.Exception (try, throw, Exception)
import Control.Applicative
import Control.Monad (join)
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

data Phase = Partial | Final

infixl 3 :-
type family phase :- field where
  'Partial :- field = Maybe (Last field)
  'Final :- field = field

data AppConfig (p :: Phase) = AppConfig
  { host :: p :- Host
  , port :: p :- Port
  , taskStoreConfig :: p :- TaskStoreConfig p
  }

type CompleteAppConfig = AppConfig 'Final
deriving instance Generic CompleteAppConfig
deriving instance Eq CompleteAppConfig
deriving instance Show CompleteAppConfig
deriving instance FromJSON CompleteAppConfig

type PartialAppConfig = AppConfig 'Partial
deriving instance Generic PartialAppConfig
deriving instance Eq PartialAppConfig
deriving instance Show PartialAppConfig
deriving instance FromJSON PartialAppConfig

defaultHost :: Host
defaultHost = "localhost"

defaultPort :: Port
defaultPort = 5000

instance Default PartialAppConfig where
  def = AppConfig Nothing Nothing Nothing

partialConfigDefaults :: PartialAppConfig
partialConfigDefaults = AppConfig
  (Just $ Last defaultHost)
  (Just $ Last defaultPort)
  (Just $ Last $ TaskStoreConfig (Just $ Last defaultTscDBFilePath))

data ConfigurationError = ConfigParseError String | InvalidPath FP.FilePath | IncompleteConfig
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
  fromENV (ProcessEnvironment env) = pure $ Right $ AppConfig { host = Last <$> host
                                                              , port = Last <$> port
                                                              , taskStoreConfig = Just $ Last $ tsConfig
                                                              }
    where
      host :: Maybe Host
      host = lookup "TODO_HOST" env

      port :: Maybe Port
      port = readMaybe =<< lookup "TODO_PORT" env

      tsConfig :: PartialTaskStoreConfig
      tsConfig = case lookup "TODO_DB" env of
                   Nothing -> def
                   Just p -> (def :: PartialTaskStoreConfig) { tscDBFilePath = Just $ Last p }

instance Semigroup PartialAppConfig where
  AppConfig { host = hA, port = pA, taskStoreConfig = tA } <> AppConfig { host = hB, port = pB, taskStoreConfig = tB } =
    AppConfig { host = hA <> hB, port = pA <> pB, taskStoreConfig = joinTSC tA tB }
    where
      joinTSC :: Maybe (Last PartialTaskStoreConfig) -> Maybe (Last PartialTaskStoreConfig) -> Maybe (Last PartialTaskStoreConfig)
      joinTSC Nothing b = b
      joinTSC a Nothing = a
      joinTSC (Just (Last a)) (Just (Last b)) = Just $ Last (a <> b)

rightOrThrow :: (Exception a) => Either a b -> IO b
rightOrThrow (Left e) = throw e
rightOrThrow (Right e) = pure e

buildCompleteConfig :: NonEmpty PartialAppConfig -> Either ConfigurationError CompleteAppConfig
buildCompleteConfig partials = case AppConfig <$> h <*> p <*> d of
                                 Nothing -> Left IncompleteConfig
                                 Just c -> Right c
  where
    merged = sconcat partials
    h = getLast <$> host merged
    p = getLast <$> port merged
    d = buildTaskStoreConfig (getLast <$> taskStoreConfig merged)

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
      fileCfgs <- case configPath of
        Nothing -> pure []
        Just path -> do
          res <- fileResult path >>= rightOrThrow
          pure [res]
      envCfg <- envResult >>= rightOrThrow
      let partials = partialConfigDefaults :| (fileCfgs ++ [envCfg])
      pure (buildCompleteConfig partials) >>= rightOrThrow

data TaskStoreConfig f = TaskStoreConfig { tscDBFilePath :: f :- FilePath }

type CompleteTaskStoreConfig = TaskStoreConfig 'Final
deriving instance Generic CompleteTaskStoreConfig
deriving instance Eq CompleteTaskStoreConfig
deriving instance Show CompleteTaskStoreConfig
deriving instance FromJSON CompleteTaskStoreConfig

type PartialTaskStoreConfig = TaskStoreConfig 'Partial
deriving instance Generic PartialTaskStoreConfig
deriving instance Eq PartialTaskStoreConfig
deriving instance Show PartialTaskStoreConfig
deriving instance FromJSON PartialTaskStoreConfig

instance Default PartialTaskStoreConfig where
  def = TaskStoreConfig $ Nothing

defaultTscDBFilePath :: FilePath
defaultTscDBFilePath = "/tmp/todo.db"

instance Semigroup PartialTaskStoreConfig where
  TaskStoreConfig pA <> TaskStoreConfig pB = TaskStoreConfig (pA <> pB)

buildTaskStoreConfig :: Maybe PartialTaskStoreConfig -> Maybe CompleteTaskStoreConfig
buildTaskStoreConfig part = Just $ TaskStoreConfig path
  where
    path = case join ((fmap getLast . tscDBFilePath) <$> part) of
      Just p -> p
      Nothing -> defaultTscDBFilePath
