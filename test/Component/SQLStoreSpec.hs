module SQLStoreSpec where

import Test.Hspec

import Config
import Component
import SQLStore

spec :: Spec
spec = do
  describe "it" $ do
    it "lol" $ do
      1 `shouldBe` 1
