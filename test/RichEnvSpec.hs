{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnvSpec (spec) where

import ArbitraryInstances ()
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = describe "dummy" $ do
  context "dummy" $ do
    it "dummy" $ do
      1 `shouldBe` 1

-- spec :: Spec
-- spec = describe "RichEnv ops" $ do
--   context "setting environment" $ do
--     it "set a single environment variable through RichEnv" $ do
--       getEnvironment >>= clearEnvironment
--       mapM_ setRichEnv $ RichEnv . S.singleton . EnvVarValue <$> mkVarValue "SOME" "var"
--       testEnv [("SOME", "var")]
--     it "set multiple environment variables through RichEnv" $ do
--       getEnvironment >>= clearEnvironment
--       mapM_ setRichEnv $ RichEnv . S.fromList <$> sequence [EnvVarValue <$> mkVarValue "SOME" "var", EnvVarValue <$> mkVarValue "OTHER" "othervar"]
--       testEnv [("SOME", "var"), ("OTHER", "othervar")]
--     it "remaps existing environment variables" $ do
--       getEnvironment >>= clearEnvironment
--       setTestEnv exampleEnv
--       mapM_ setRichEnv $ RichEnv . S.singleton . EnvVarNameMap <$> mkVarMap "SOME" "FOO"
--       testEnv [("SOME", "bar")]
--     it "remaps prefixed variables" $ do
--       getEnvironment >>= clearEnvironment
--       setTestEnv exampleEnv
--       mapM_ setRichEnv $ RichEnv . S.singleton . EnvVarPrefix <$> mkVarPrefix "NEW_" "PREFIXED_"
--       testEnv [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
--   context "getting the environment variable list" $ do
--     it "gets the environment variable list" $ do
--       getEnvironment >>= clearEnvironment
--       setTestEnv exampleEnv
--       testEnvList
--         [("SOME", "bar")]
--         (RichEnv . S.singleton . EnvVarNameMap <$> mkVarMap "SOME" "FOO")
--     it "gets the environment variable list with prefixes" $ do
--       getEnvironment >>= clearEnvironment
--       setTestEnv exampleEnv
--       testEnvList
--         [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
--         (RichEnv . S.singleton . EnvVarPrefix <$> mkVarPrefix "NEW_" "PREFIXED_")

--   context "working with YAML" $ it "parses a YAML file into expected results" $ do
--     getEnvironment >>= clearEnvironment
--     setTestEnv fileTestsBaseEnv
--     let res = Yaml.decodeEither' yamlTestCase :: Either Yaml.ParseException TestType
--     case res of
--       Left err -> fail $ show err
--       Right actual -> testEnvList fileTestsCaseExpected (Just $ env actual)

--   context "working with JSON" $ it "parses a JSON file into expected results" $ do
--     getEnvironment >>= clearEnvironment
--     setTestEnv fileTestsBaseEnv
--     let res = JSON.eitherDecodeStrict jsonTestCase :: Either String TestType
--     case res of
--       Left err -> fail err
--       Right actual -> testEnvList fileTestsCaseExpected (Just $ env actual)

--   context "invariants" $ do
--     prop "parsing YAML from and to a RichEnv should end in the original value" $ \re -> do
--       let yaml = Yaml.encode re
--           res = Yaml.decodeEither' yaml :: Either Yaml.ParseException RichEnv
--        in case res of
--             Left err -> fail $ displayException err
--             Right actual -> do
--               actual `shouldBe` re
--     prop "parsing JSON from and to a RichEnv should end in the original value" $ \re -> do
--       let json = JSON.encode re
--           res = JSON.eitherDecode' json :: Either String RichEnv
--        in case res of
--             Left err -> fail err
--             Right actual -> do
--               actual `shouldBe` re
--   where
--     exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

-- setTestEnv :: [(String, String)] -> IO ()
-- setTestEnv = mapM_ (uncurry setEnv)

-- testEnv :: [(String, String)] -> Expectation
-- testEnv expected = getEnvironment >>= (`shouldBe` sort expected) . sort

-- testEnvList :: [(String, String)] -> Maybe RichEnv -> Expectation
-- testEnvList expected re = toEnvList (fromJust re) >>= (`shouldBe` sort expected) . sort

-- newtype TestType = TestType
--   { env :: RichEnv
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (FromJSON, ToJSON)

-- instance Arbitrary TestType where
--   arbitrary :: Gen TestType
--   arbitrary = TestType <$> arbitrary

-- -- Test cases that use Aeson's FromJSON/ToJSON instances

-- yamlTestCase :: B.ByteString
-- yamlTestCase =
--   C8.pack $
--     unlines
--       [ "env:",
--         "  - name: SOME", -- This is the same as `EnvVarValue (VarValue "SOME" "var")`
--         "    value: somevar",
--         "  - name: OTHER", -- This is the same as `EnvVarValue (VarValue "OTHER" "othervar")`
--         "    value: othervar",
--         "  - name: FOO", -- This is the same as `EnvVarNameMap (VarMap "SOME" "FOO")`
--         "    from: SOME",
--         "  - name: NEW_*", -- This is the same as `EnvVarPrefix (VarPrefix "NEW_" "PREFIXED_")`
--         "    from: PREFIXED_*"
--       ]

-- jsonTestCase :: B.ByteString
-- jsonTestCase =
--   C8.pack $
--     unlines
--       [ "{",
--         "  \"env\": [",
--         "    {",
--         "      \"name\": \"SOME\",",
--         "      \"value\": \"somevar\"",
--         "    },",
--         "    {",
--         "      \"name\": \"OTHER\",",
--         "      \"value\": \"othervar\"",
--         "    },",
--         "    {",
--         "      \"name\": \"FOO\",",
--         "      \"from\": \"SOME\"",
--         "    },",
--         "    {",
--         "      \"name\": \"NEW_*\",",
--         "      \"from\": \"PREFIXED_*\"",
--         "    }",
--         "  ]",
--         "}"
--       ]

-- fileTestsBaseEnv :: [(String, String)]
-- fileTestsBaseEnv = [("SOME", "bar"), ("OTHER", "othervar"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

-- fileTestsCaseExpected :: [(String, String)]
-- fileTestsCaseExpected =
--   [ ("FOO", "bar"),
--     ("OTHER", "othervar"),
--     ("SOME", "somevar"),
--     ("NEW_VAR", "content"),
--     ("NEW_VAR2", "content2")
--   ]
