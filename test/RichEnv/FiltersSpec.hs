module RichEnv.FiltersSpec (spec) where

import Data.HashSet qualified as S
import Data.Maybe (catMaybes)
import RichEnv.Filters (varMaps, varPrefixes, varValues)
import RichEnv.Types.RichEnv (RichEnv (..), RichEnvItem (..))
import RichEnv.Types.VarMap (mkVarMap)
import RichEnv.Types.VarPrefix (mkVarPrefix)
import RichEnv.Types.VarValue (mkVarValue)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "varMaps" $ do
    it "returns an empty set when given an empty set" $ varMaps mempty `shouldBe` mempty

    it "returns only VarMap items" $ do
      let vv = mkVarValue "foo" "bar"
          vm = mkVarMap "bar" "baz"
          vp = mkVarPrefix "qux" "quux"
          items = catMaybes [EnvVarValue <$> vv, EnvVarNameMap <$> vm, EnvVarPrefix <$> vp]
          testRichEnv = RichEnv $ S.fromList items
          expected = S.fromList $ catMaybes [vm]
      varMaps testRichEnv `shouldBe` expected

  describe "varValues" $ do
    it "returns an empty set when given an empty set" $ varValues mempty `shouldBe` mempty

    it "returns only VarValue items" $ do
      let vv = mkVarValue "foo" "bar"
          vm = mkVarMap "bar" "baz"
          vp = mkVarPrefix "qux" "quux"
          items = catMaybes [EnvVarValue <$> vv, EnvVarNameMap <$> vm, EnvVarPrefix <$> vp]
          testRichEnv = RichEnv $ S.fromList items
          expected = S.fromList $ catMaybes [vv]
      varValues testRichEnv `shouldBe` expected

  describe "varPrefixes" $ do
    it "returns an empty set when given an empty set" $ varPrefixes mempty `shouldBe` S.empty

    it "returns only VarPrefix items" $ do
      let vv = mkVarValue "foo" "bar"
          vm = mkVarMap "bar" "baz"
          vp = mkVarPrefix "qux" "quux"
          items = catMaybes [EnvVarValue <$> vv, EnvVarNameMap <$> vm, EnvVarPrefix <$> vp]
          testRichEnv = RichEnv $ S.fromList items
          expected = S.fromList $ catMaybes [vp]
      varPrefixes testRichEnv `shouldBe` expected
