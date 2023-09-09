module RichEnv.FiltersSpec (spec) where

import Data.HashSet qualified as S
import Data.List.NonEmpty (fromList)
import RichEnv.Filters (varMaps, varPrefixes, varValues)
import RichEnv.Types (RichEnvItem (..), VarPrefix (..), VarValue (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Utils (nonEmptyVarMap, nonEmptyVarValue)

spec :: Spec
spec = do
  describe "varMaps" $ do
    it "returns an empty set when given an empty set" $ do
      varMaps S.empty `shouldBe` S.empty

    it "returns only VarMap items" $ do
      let richEnv = S.fromList [EnvVarValue (nonEmptyVarValue "foo" "bar"), EnvVarNameMap (nonEmptyVarMap "bar" "baz"), EnvVarPrefix (VarPrefix "qux" "quux")]
      varMaps richEnv `shouldBe` S.fromList [nonEmptyVarMap "bar" "baz"]

  describe "varValues" $ do
    it "returns an empty set when given an empty set" $ do
      varValues S.empty `shouldBe` S.empty

    it "returns only VarValue items" $ do
      let richEnv = S.fromList [EnvVarValue (VarValue (fromList "foo") "bar"), EnvVarNameMap (nonEmptyVarMap "bar" "baz"), EnvVarPrefix (VarPrefix "qux" "quux")]
      varValues richEnv `shouldBe` S.fromList [nonEmptyVarValue "foo" "bar"]

  describe "varPrefixes" $ do
    it "returns an empty set when given an empty set" $ do
      varPrefixes S.empty `shouldBe` S.empty

    it "returns only VarPrefix items" $ do
      let richEnv = S.fromList [EnvVarValue (VarValue (fromList "foo") "bar"), EnvVarNameMap (nonEmptyVarMap "bar" "baz"), EnvVarPrefix (VarPrefix "qux" "quux")]
      varPrefixes richEnv `shouldBe` S.fromList [VarPrefix "qux" "quux"]
