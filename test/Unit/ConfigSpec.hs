module App.ConfigSpec(spec) where

import Test.HSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Fake test" $ do
    it "passes" $ do
      True `shouldBe` True
