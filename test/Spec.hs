module Main (main) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "api" $ do
    it "should work" $ do
      True `shouldBe` True
