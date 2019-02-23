{-# LANGUAGE CPP #-}

module ConfigSpec(spec) where

import Control.Monad.IO.Class
import Test.Hspec
import Config as C
import Data.Default (def)

import System.FilePath

testJSON :: FilePath
testJSON = takeDirectory __FILE__ </> "test.json"

testTOML :: FilePath
testTOML = takeDirectory __FILE__ </> "test.toml"

spec :: Spec
spec = do
  describe "defaults" $ do
    it "has 'localhost' as default host" $ do
      cfg <- makeAppConfig (ProcessEnvironment []) Nothing >>= rightOrThrow
      host cfg `shouldBe` "localhost"
    it "has 5000 as default port" $ do
      cfg <- makeAppConfig (ProcessEnvironment []) Nothing >>= rightOrThrow
      port cfg `shouldBe` 5000

  describe "env config" $ do
    it "is used for every field" $ do
      cfg <- makeAppConfig (ProcessEnvironment [("TODO_HOST", "this-test"), ("TODO_PORT", "6743")]) Nothing >>= rightOrThrow
      port cfg `shouldBe` 6743
      host cfg `shouldBe` "this-test"

  describe "toml config" $ do
    it "can be parsed" $ do
      cfg <- makeAppConfig (ProcessEnvironment []) (Just testTOML) >>= rightOrThrow
      host cfg `shouldBe` "host-from-toml"

  describe "json config" $ do
    it "can be parsed" $ do
      cfg <- makeAppConfig (ProcessEnvironment []) (Just testJSON) >>= rightOrThrow
      port cfg `shouldBe` 13822

  describe "combining configs" $ do
    it "env is more important" $ do
      cfg <- makeAppConfig (ProcessEnvironment [("TODO_HOST", "this-test"), ("TODO_PORT", "6743")]) (Just testTOML) >>= rightOrThrow
      port cfg `shouldBe` 6743
      host cfg `shouldBe` "this-test"
    it "env and file configs are combined" $ do
      cfg <- makeAppConfig (ProcessEnvironment [("TODO_PORT", "6743")]) (Just testTOML) >>= rightOrThrow
      port cfg `shouldBe` 6743
      host cfg `shouldBe` "host-from-toml"

completeAppDefault :: CompleteAppConfig
completeAppDefault = undefined

partialAppDefault :: PartialAppConfig
partialAppDefault = def
