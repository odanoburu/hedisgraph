{-# LANGUAGE QuasiQuotes #-}
module Database.Bolt.CompatibilitySpec (main, spec) where

import Test.Hspec

import Database.Bolt.Compatibility
import qualified Database.Bolt as Bolt
import qualified Database.RedisGraph as RedisG

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Decoding tests" $ do
    it "null value"
      $ redisValue (Bolt.N ())
      `shouldBe` RedisG.N
