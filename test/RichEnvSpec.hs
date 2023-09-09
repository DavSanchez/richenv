module RichEnvSpec (spec) where

import Data.HashSet qualified as S
import Data.List (sort)
import RichEnv (clearEnvironment, setRichEnv, toEnvList)
import RichEnv.Types (RichEnv, RichEnvItem (..), VarPrefix (..))
import System.Environment (getEnvironment, setEnv)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.QuickCheck ()
import Utils (nonEmptyVarMap, nonEmptyVarValue)

spec :: Spec
spec = do
  describe "RichEnv ops" $ do
    it "set a single environment variable through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      setRichEnv richEnv1
      testEnv expectedEnv1
    it "set multiple environment variables through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      setRichEnv richEnv2
      testEnv expectedEnv2
    it "remaps existing environment variables" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      setRichEnv richEnvMapping
      testEnv expectedMapped
    it "remaps prefixed variables" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      setRichEnv richEnvPrefix
      testEnv expectedNewPrefix
  describe "getting the environment variable list" $ do
    it "gets the environment variable list" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      testEnvList richEnvMapping expectedMapped
    it "gets the environment variable list with prefixes" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      testEnvList richEnvPrefix expectedNewPrefix

setTestEnv :: [(String, String)] -> IO ()
setTestEnv = mapM_ (uncurry setEnv)

testEnv :: [(String, String)] -> Expectation
testEnv expected = getEnvironment >>= (`shouldBe` sort expected) . sort

testEnvList :: RichEnv -> [(String, String)] -> Expectation
testEnvList re expected = toEnvList re >>= (`shouldBe` sort expected) . sort

-- Test cases

richEnv1 :: RichEnv
richEnv1 = S.singleton (EnvVarValue (nonEmptyVarValue "SOME" "var"))

expectedEnv1 :: [(String, String)]
expectedEnv1 = [("SOME", "var")]

richEnv2 :: RichEnv
richEnv2 = S.fromList [EnvVarValue (nonEmptyVarValue "SOME" "var"), EnvVarValue (nonEmptyVarValue "OTHER" "othervar")]

expectedEnv2 :: [(String, String)]
expectedEnv2 = [("SOME", "var"), ("OTHER", "othervar")]

exampleEnv :: [(String, String)]
exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

richEnvMapping :: RichEnv
richEnvMapping = S.singleton $ EnvVarNameMap (nonEmptyVarMap "SOME" "FOO")

expectedMapped :: [(String, String)]
expectedMapped = [("SOME", "bar")]

richEnvPrefix :: RichEnv
richEnvPrefix = S.singleton $ EnvVarPrefix (VarPrefix "NEW_" "PREFIXED_")

expectedNewPrefix :: [(String, String)]
expectedNewPrefix = [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
