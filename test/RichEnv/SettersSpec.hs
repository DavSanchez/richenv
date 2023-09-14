module RichEnv.SettersSpec (spec) where

import Data.HashMap.Strict qualified as HM
import RichEnv.Setters (mappingsToValues, prefixesToValues)
import RichEnv.Types (Environment)
import RichEnv.Types.RichEnv (Mappings (..), Prefixes (..), Values (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "mappings" $ do
    it "returns an empty environment when given an empty set" $ do
      mappingsToValues mempty mempty `shouldBe` mempty
    it "remaps a single environment variable" $ do
      mappingsToValues exampleEnv testVarMap `shouldBe` expectedVars
  describe "prefixes" $ do
    it "returns an empty environment when given an empty set" $ do
      prefixesToValues mempty mempty `shouldBe` mempty
    it "remaps a single environment variable" $ do
      prefixesToValues exampleEnv testVarPrefix `shouldBe` expectedPrefixedVars
    it "passes all environment variables adding a prefix" $ do
      prefixesToValues exampleEnv passAllAndAddPrefix `shouldBe` expectedAllPrefixedVars
    it "remaps prefixed environment variables removing the prefix" $ do
      prefixesToValues exampleEnv passSomeAndRemovePrefix `shouldBe` expectedSomePrefixedVars
    it "passes only prefixed environment variables preserving the prefix" $ do
      prefixesToValues exampleEnv passOnlyPrefixedPreservingPrefix `shouldBe` expectedOnlyPrefixedPreservingPrefix
    it "passes all environment variables" $ do
      prefixesToValues exampleEnv passAll `shouldBe` expectedAll

exampleEnv :: Environment
exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

testVarMap :: Mappings
testVarMap = Mappings $ HM.singleton "SOME" "FOO"

expectedVars :: Values
expectedVars = Values $ HM.singleton "SOME" "bar"

testVarPrefix :: Prefixes
testVarPrefix = Prefixes $ HM.singleton "NEW_" ["PREFIXED_"]

expectedPrefixedVars :: Values
expectedPrefixedVars = Values $ HM.fromList [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]

passAllAndAddPrefix :: Prefixes
passAllAndAddPrefix = Prefixes $ HM.singleton "NEWPREFIX_" [""]

expectedAllPrefixedVars :: Values
expectedAllPrefixedVars = Values $ HM.fromList [("NEWPREFIX_FOO", "bar"), ("NEWPREFIX_BAZ", "qux"), ("NEWPREFIX_PREFIXED_VAR", "content"), ("NEWPREFIX_PREFIXED_VAR2", "content2")]

passSomeAndRemovePrefix :: Prefixes
passSomeAndRemovePrefix = Prefixes $ HM.singleton "" ["PREFIXED_"]

expectedSomePrefixedVars :: Values
expectedSomePrefixedVars = Values $ HM.fromList [("VAR", "content"), ("VAR2", "content2")]

passOnlyPrefixedPreservingPrefix :: Prefixes
passOnlyPrefixedPreservingPrefix = Prefixes $ HM.singleton "PREFIXED_" ["PREFIXED_"]

expectedOnlyPrefixedPreservingPrefix :: Values
expectedOnlyPrefixedPreservingPrefix = Values $ HM.fromList [("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

passAll :: Prefixes
passAll = Prefixes $ HM.singleton mempty mempty

expectedAll :: Values
expectedAll = Values $ HM.fromList [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]
