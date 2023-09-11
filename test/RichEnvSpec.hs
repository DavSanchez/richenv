{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module RichEnvSpec (spec) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.HashSet qualified as S
import Data.List (sort)
import Data.Yaml (FromJSON, ParseException, decodeEither')
import GHC.Generics (Generic)
import RichEnv (clearEnvironment, setRichEnv, toEnvList)
import RichEnv.Types (RichEnv, RichEnvItem (..), VarPrefix (..))
import System.Environment (getEnvironment, setEnv)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Utils (nonEmptyVarMap, nonEmptyVarValue)

spec :: Spec
spec = do
  describe "RichEnv ops: set environment" $ do
    it "set a single environment variable through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      setRichEnv $ S.singleton (EnvVarValue (nonEmptyVarValue "SOME" "var"))
      testEnv [("SOME", "var")]
    it "set multiple environment variables through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      setRichEnv $ S.fromList [EnvVarValue (nonEmptyVarValue "SOME" "var"), EnvVarValue (nonEmptyVarValue "OTHER" "othervar")]
      testEnv [("SOME", "var"), ("OTHER", "othervar")]
    it "remaps existing environment variables" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      setRichEnv $ S.singleton $ EnvVarNameMap (nonEmptyVarMap "SOME" "FOO")
      testEnv [("SOME", "bar")]
    it "remaps prefixed variables" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      setRichEnv $ S.singleton $ EnvVarPrefix (VarPrefix "NEW_" "PREFIXED_")
      testEnv [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
  describe "RichEnv ops: getting the environment variable list" $ do
    it "gets the environment variable list" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      testEnvList
        [("SOME", "bar")]
        (S.singleton $ EnvVarNameMap (nonEmptyVarMap "SOME" "FOO"))
    it "gets the environment variable list with prefixes" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      testEnvList
        [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
        (S.singleton $ EnvVarPrefix (VarPrefix "NEW_" "PREFIXED_"))
  describe "RichEnv ops: From YAML" $ do
    it "parses a YAML file into expected results" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv yamlBaseEnv
      let res = decodeEither' yamlTestCase :: Either ParseException YamlTest
      case res of
        Left err -> fail $ show err
        Right actual -> testEnvList yamlTestCaseExpected (env actual)
  where
    exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

setTestEnv :: [(String, String)] -> IO ()
setTestEnv = mapM_ (uncurry setEnv)

testEnv :: [(String, String)] -> Expectation
testEnv expected = getEnvironment >>= (`shouldBe` sort expected) . sort

testEnvList :: [(String, String)] -> RichEnv -> Expectation
testEnvList expected re = toEnvList re >>= (`shouldBe` sort expected) . sort

-- YAML test cases that use the JSON conversion instances from Aeson

newtype YamlTest = YamlTest {env :: RichEnv}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

yamlTestCase :: B.ByteString
yamlTestCase =
  C8.pack $
    unlines
      [ "env:",
        "  - name: SOME", -- This is the same as `EnvVarValue (VarValue "SOME" "var")`
        "    value: somevar",
        "  - name: OTHER", -- This is the same as `EnvVarValue (VarValue "OTHER" "othervar")`
        "    value: othervar",
        "  - name: FOO", -- This is the same as `EnvVarNameMap (VarMap "SOME" "FOO")`
        "    from: SOME",
        "  - name: NEW_*", -- This is the same as `EnvVarPrefix (VarPrefix "NEW_" "PREFIXED_")`
        "    from: PREFIXED_*"
      ]

yamlBaseEnv :: [(String, String)]
yamlBaseEnv = [("SOME", "bar"), ("OTHER", "othervar"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

yamlTestCaseExpected :: [(String, String)]
yamlTestCaseExpected =
  [ ("FOO", "bar"),
    ("OTHER", "othervar"),
    ("SOME", "somevar"),
    ("NEW_VAR", "content"),
    ("NEW_VAR2", "content2")
  ]
