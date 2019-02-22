module ConfigSpec(spec) where

import Test.Hspec
import Config as C
import Data.Default (def)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "defaults" $ do
    it "has 'localhost' as default host" $ host completeAppDefault `shouldBe` "localhost"
    it "has 5000 as default port" $ port completeAppDefault `shouldBe` 5000

completeAppDefault :: CompleteAppConfig
completeAppDefault = def

partialAppDefault :: PartialAppConfig
partialAppDefault = def
