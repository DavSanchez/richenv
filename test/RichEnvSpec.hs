module RichEnvSpec (spec) where

import System.Environment (getEnvironment, unsetEnv)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "api" $ do
        it "should work" $ do
            True `shouldBe` True

clearEnv :: IO ()
clearEnv = getEnvironment >>= mapM_ (unsetEnv . fst)
