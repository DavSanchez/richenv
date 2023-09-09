module RichEnv.SettersSpec (spec) where

import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import RichEnv.Setters (setPrefixedVars, setVarMapValues)
import RichEnv.Types (Environment, VarMap (..), VarPrefix (..), VarValue (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Utils (nonEmptyVarMap, nonEmptyVarValue)

spec :: Spec
spec = do
  describe "varMaps" $ do
    it "returns an empty environment when given an empty set" $ do
      setVarMapValues mempty mempty `shouldBe` mempty
    it "remaps a single environment variable" $ do
      setVarMapValues exampleEnv testVarMap `shouldBe` expectedVars
  describe "varPrefixes" $ do
    it "returns an empty environment when given an empty set" $ do
      setPrefixedVars mempty mempty `shouldBe` mempty
    it "remaps a single environment variable" $ do
      setPrefixedVars exampleEnv testVarPrefix `shouldBe` expectedPrefixedVars
    it "passes all environment variables adding a prefix" $ do
      setPrefixedVars exampleEnv passAllAndAddPrefix `shouldBe` expectedAllPrefixedVars
    it "remaps prefixed environment variables removing the prefix" $ do
      setPrefixedVars exampleEnv passSomeAndRemovePrefix `shouldBe` expectedSomePrefixedVars
    it "passes only prefixed environment variables preserving the prefix" $ do
      setPrefixedVars exampleEnv passOnlyPrefixedPreservingPrefix `shouldBe` expectedOnlyPrefixedPreservingPrefix
    it "passes all environment variables" $ do
      setPrefixedVars exampleEnv passAll `shouldBe` expectedAll

exampleEnv :: Environment
exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

testVarMap :: HashSet VarMap
testVarMap = S.singleton $ nonEmptyVarMap "SOME" "FOO"

expectedVars :: HashSet VarValue
expectedVars = S.singleton $ nonEmptyVarValue "SOME" "bar"

testVarPrefix :: HashSet VarPrefix
testVarPrefix = S.singleton $ VarPrefix "NEW_" "PREFIXED_"

expectedPrefixedVars :: HashSet VarValue
expectedPrefixedVars = S.fromList [nonEmptyVarValue "NEW_VAR" "content", nonEmptyVarValue "NEW_VAR2" "content2"]

passAllAndAddPrefix :: HashSet VarPrefix
passAllAndAddPrefix = S.singleton $ VarPrefix "NEWPREFIX_" ""

expectedAllPrefixedVars :: HashSet VarValue
expectedAllPrefixedVars = S.fromList [nonEmptyVarValue "NEWPREFIX_FOO" "bar", nonEmptyVarValue "NEWPREFIX_BAZ" "qux", nonEmptyVarValue "NEWPREFIX_PREFIXED_VAR" "content", nonEmptyVarValue "NEWPREFIX_PREFIXED_VAR2" "content2"]

passSomeAndRemovePrefix :: HashSet VarPrefix
passSomeAndRemovePrefix = S.singleton $ VarPrefix "" "PREFIXED_"

expectedSomePrefixedVars :: HashSet VarValue
expectedSomePrefixedVars = S.fromList [nonEmptyVarValue "VAR" "content", nonEmptyVarValue "VAR2" "content2"]

passOnlyPrefixedPreservingPrefix :: HashSet VarPrefix
passOnlyPrefixedPreservingPrefix = S.singleton $ VarPrefix "PREFIXED_" "PREFIXED_"

expectedOnlyPrefixedPreservingPrefix :: HashSet VarValue
expectedOnlyPrefixedPreservingPrefix = S.fromList [nonEmptyVarValue "PREFIXED_VAR" "content", nonEmptyVarValue "PREFIXED_VAR2" "content2"]

passAll :: HashSet VarPrefix
passAll = S.singleton $ VarPrefix "" ""

expectedAll :: HashSet VarValue
expectedAll = S.fromList [nonEmptyVarValue "FOO" "bar", nonEmptyVarValue "BAZ" "qux", nonEmptyVarValue "PREFIXED_VAR" "content", nonEmptyVarValue "PREFIXED_VAR2" "content2"]
