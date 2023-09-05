module RichEnv.SettersSpec (spec) where

import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.Text (Text)
import RichEnv.Setters (setPrefixedVars, setVarMapValues)
import RichEnv.Types (VarMap (..), VarPrefix (..), VarValue (..))
import Test.Hspec (Spec, describe, it, shouldBe)

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

exampleEnv :: [(Text, Text)]
exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

testVarMap :: HashSet VarMap
testVarMap = S.singleton (VarMap "SOME" "FOO")

expectedVars :: HashSet VarValue
expectedVars = S.singleton (VarValue "SOME" "bar")

testVarPrefix :: HashSet VarPrefix
testVarPrefix = S.singleton (VarPrefix "NEW_" "PREFIXED_")

expectedPrefixedVars :: HashSet VarValue
expectedPrefixedVars = S.fromList [VarValue "NEW_VAR" "content", VarValue "NEW_VAR2" "content2"]
