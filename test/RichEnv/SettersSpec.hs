module RichEnv.SettersSpec (spec) where

import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import RichEnv.Setters (setPrefixedVars, setVarMapValues)
import RichEnv.Types (Environment)
import RichEnv.Types.VarMap (VarMap (..), mkVarMap)
import RichEnv.Types.VarPrefix (VarPrefix (..), mkVarPrefix)
import RichEnv.Types.VarValue (VarValue (..), mkVarValue)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "varMaps" $ do
    it "returns an empty environment when given an empty set" $ do
      setVarMapValues mempty mempty `shouldBe` mempty
    it "remaps a single environment variable" $ do
      setVarMapValues exampleEnv <$> testVarMap `shouldBe` expectedVars
  describe "varPrefixes" $ do
    it "returns an empty environment when given an empty set" $ do
      setPrefixedVars mempty mempty `shouldBe` mempty
    it "remaps a single environment variable" $ do
      setPrefixedVars exampleEnv <$> testVarPrefix `shouldBe` expectedPrefixedVars
    it "passes all environment variables adding a prefix" $ do
      setPrefixedVars exampleEnv <$> passAllAndAddPrefix `shouldBe` expectedAllPrefixedVars
    it "remaps prefixed environment variables removing the prefix" $ do
      setPrefixedVars exampleEnv <$> passSomeAndRemovePrefix `shouldBe` expectedSomePrefixedVars
    it "passes only prefixed environment variables preserving the prefix" $ do
      setPrefixedVars exampleEnv <$> passOnlyPrefixedPreservingPrefix `shouldBe` expectedOnlyPrefixedPreservingPrefix
    it "passes all environment variables" $ do
      setPrefixedVars exampleEnv <$> passAll `shouldBe` expectedAll

exampleEnv :: Environment
exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

testVarMap :: Maybe (HashSet VarMap)
testVarMap = S.singleton <$> mkVarMap "SOME" "FOO"

expectedVars :: Maybe (HashSet VarValue)
expectedVars = S.singleton <$> mkVarValue "SOME" "bar"

testVarPrefix :: Maybe (HashSet VarPrefix)
testVarPrefix = S.singleton <$> mkVarPrefix "NEW_" "PREFIXED_"

expectedPrefixedVars :: Maybe (HashSet VarValue)
expectedPrefixedVars = S.fromList <$> sequence [mkVarValue "NEW_VAR" "content", mkVarValue "NEW_VAR2" "content2"]

passAllAndAddPrefix :: Maybe (HashSet VarPrefix)
passAllAndAddPrefix = S.singleton <$> mkVarPrefix "NEWPREFIX_" ""

expectedAllPrefixedVars :: Maybe (HashSet VarValue)
expectedAllPrefixedVars = S.fromList <$> sequence [mkVarValue "NEWPREFIX_FOO" "bar", mkVarValue "NEWPREFIX_BAZ" "qux", mkVarValue "NEWPREFIX_PREFIXED_VAR" "content", mkVarValue "NEWPREFIX_PREFIXED_VAR2" "content2"]

passSomeAndRemovePrefix :: Maybe (HashSet VarPrefix)
passSomeAndRemovePrefix = S.singleton <$> mkVarPrefix "" "PREFIXED_"

expectedSomePrefixedVars :: Maybe (HashSet VarValue)
expectedSomePrefixedVars = S.fromList <$> sequence [mkVarValue "VAR" "content", mkVarValue "VAR2" "content2"]

passOnlyPrefixedPreservingPrefix :: Maybe (HashSet VarPrefix)
passOnlyPrefixedPreservingPrefix = S.singleton <$> mkVarPrefix "PREFIXED_" "PREFIXED_"

expectedOnlyPrefixedPreservingPrefix :: Maybe (HashSet VarValue)
expectedOnlyPrefixedPreservingPrefix = S.fromList <$> sequence [mkVarValue "PREFIXED_VAR" "content", mkVarValue "PREFIXED_VAR2" "content2"]

passAll :: Maybe (HashSet VarPrefix)
passAll = S.singleton <$> mkVarPrefix mempty mempty

expectedAll :: Maybe (HashSet VarValue)
expectedAll = S.fromList <$> sequence [mkVarValue "FOO" "bar", mkVarValue "BAZ" "qux", mkVarValue "PREFIXED_VAR" "content", mkVarValue "PREFIXED_VAR2" "content2"]
