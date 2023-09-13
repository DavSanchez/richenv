{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnvSpec (spec) where

import ArbitraryInstances ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.HashSet qualified as S
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import RichEnv (clearEnvironment, setRichEnv, toEnvList)
import RichEnv.Types.RichEnv (RichEnv (..), RichEnvItem (..))
import RichEnv.Types.VarMap (mkVarMap)
import RichEnv.Types.VarPrefix (mkVarPrefix)
import RichEnv.Types.VarValue (mkVarValue)
import System.Environment (getEnvironment, setEnv)
import Test.Hspec (Expectation, Spec, context, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Gen)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

spec :: Spec
spec = describe "RichEnv ops" $ do
  context "setting environment" $ do
    it "set a single environment variable through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      mapM_ setRichEnv $ RichEnv . S.singleton . EnvVarValue <$> mkVarValue "SOME" "var"
      testEnv [("SOME", "var")]
    it "set multiple environment variables through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      mapM_ setRichEnv $ RichEnv . S.fromList <$> sequence [EnvVarValue <$> mkVarValue "SOME" "var", EnvVarValue <$> mkVarValue "OTHER" "othervar"]
      testEnv [("SOME", "var"), ("OTHER", "othervar")]
    it "remaps existing environment variables" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      mapM_ setRichEnv $ RichEnv . S.singleton . EnvVarNameMap <$> mkVarMap "SOME" "FOO"
      testEnv [("SOME", "bar")]
    it "remaps prefixed variables" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      mapM_ setRichEnv $ RichEnv . S.singleton . EnvVarPrefix <$> mkVarPrefix "NEW_" "PREFIXED_"
      testEnv [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
  context "getting the environment variable list" $ do
    it "gets the environment variable list" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      testEnvList
        [("SOME", "bar")]
        (RichEnv . S.singleton . EnvVarNameMap <$> mkVarMap "SOME" "FOO")
    it "gets the environment variable list with prefixes" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      testEnvList
        [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
        (RichEnv . S.singleton . EnvVarPrefix <$> mkVarPrefix "NEW_" "PREFIXED_")

  context "working with YAML" $ it "parses a YAML file into expected results" $ do
    getEnvironment >>= clearEnvironment
    setTestEnv yamlBaseEnv
    let res = Yaml.decodeEither' yamlTestCase :: Either Yaml.ParseException TestType
    case res of
      Left err -> fail $ show err
      Right actual -> testEnvList yamlTestCaseExpected (Just $ env actual)

  context "invariants" $ do
    prop "parsing YAML from and to a RichEnv should end in the original value" $ \re -> do
      -- putStrLn $ "SEED VALUE: " <> show re
      let yaml = Yaml.encode re
          res = Yaml.decodeEither' yaml :: Either Yaml.ParseException RichEnv
       in case res of
            Left err -> fail $ show err
            Right actual -> do
              -- putStrLn $ "ACTUAL VALUE: " <> show actual
              actual `shouldBe` re
    prop "parsing JSON from and to a RichEnv should end in the original value" $ \re -> do
      -- putStrLn $ "SEED VALUE: " <> show re
      let json = JSON.encode re
          res = JSON.eitherDecode' json :: Either String RichEnv
       in case res of
            Left err -> fail err
            Right actual -> do
              -- putStrLn $ "ACTUAL VALUE: " <> show actual
              actual `shouldBe` re
  where
    exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

setTestEnv :: [(String, String)] -> IO ()
setTestEnv = mapM_ (uncurry setEnv)

testEnv :: [(String, String)] -> Expectation
testEnv expected = getEnvironment >>= (`shouldBe` sort expected) . sort

testEnvList :: [(String, String)] -> Maybe RichEnv -> Expectation
testEnvList expected re = toEnvList (fromJust re) >>= (`shouldBe` sort expected) . sort

newtype TestType = TestType
  { env :: RichEnv
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary TestType where
  arbitrary :: Gen TestType
  arbitrary = TestType <$> arbitrary

-- YAML test cases that use the JSON conversion instances from Aeson

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

{-

env:
  SOME: somevar
  OTHER: othervar
  FOO: env.SOME
  NEW_*: env.PREFIXED_*
  CMD_VAR: $(curl -X POST ...)

-}

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
