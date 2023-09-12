module RichEnv.FiltersSpec (spec) where

import Data.HashSet qualified as S
import Data.List.NonEmpty (fromList)
import RichEnv.Filters (varMaps, varPrefixes, varValues)
import RichEnv.Types (RichEnv (..), RichEnvItem (..), VarPrefix (..), VarValue (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Utils (nonEmptyVarMap, nonEmptyVarValue)

spec :: Spec
spec = do
  describe "varMaps" $ do
    it "returns an empty set when given an empty set" $ do
      varMaps mempty `shouldBe` mempty

    it "returns only VarMap items" $ do
      let testRichEnv = RichEnv $ S.fromList [EnvVarValue (nonEmptyVarValue "foo" "bar"), EnvVarNameMap (nonEmptyVarMap "bar" "baz"), EnvVarPrefix (VarPrefix "qux" "quux")]
      varMaps testRichEnv `shouldBe` S.fromList [nonEmptyVarMap "bar" "baz"]

  describe "varValues" $ do
    it "returns an empty set when given an empty set" $ do
      varValues mempty `shouldBe` mempty

    it "returns only VarValue items" $ do
      let testRichEnv = RichEnv $ S.fromList [EnvVarValue (VarValue (fromList "foo") "bar"), EnvVarNameMap (nonEmptyVarMap "bar" "baz"), EnvVarPrefix (VarPrefix "qux" "quux")]
      varValues testRichEnv `shouldBe` S.fromList [nonEmptyVarValue "foo" "bar"]

  describe "varPrefixes" $ do
    it "returns an empty set when given an empty set" $ do
      varPrefixes mempty `shouldBe` S.empty

    it "returns only VarPrefix items" $ do
      let testRichEnv = RichEnv $ S.fromList [EnvVarValue (VarValue (fromList "foo") "bar"), EnvVarNameMap (nonEmptyVarMap "bar" "baz"), EnvVarPrefix (VarPrefix "qux" "quux")]
      varPrefixes testRichEnv `shouldBe` S.fromList [VarPrefix "qux" "quux"]
