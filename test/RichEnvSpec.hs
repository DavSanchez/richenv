module RichEnvSpec (spec) where

import Data.HashSet qualified as S
import Data.List (sort)
import RichEnv (clearEnvironment, richEnv)
import RichEnv.Types (RichEnv, RichEnvItem (..), VarMap (..), VarValue (..))
import System.Environment (getEnvironment, setEnv)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.QuickCheck ()

spec :: Spec
spec = do
  describe "RichEnv ops" $ do
    it "set a single environment variable through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      richEnv richEnv1
      testEnv expectedEnv1
    it "set multiple environment variables through RichEnv" $ do
      getEnvironment >>= clearEnvironment
      richEnv richEnv2
      testEnv expectedEnv2
    it "remaps existing environment variables" $ do
      getEnvironment >>= clearEnvironment
      setTestEnv exampleEnv
      richEnv richEnv3
      testEnv expectedEnv3

setTestEnv :: [(String, String)] -> IO ()
setTestEnv = mapM_ (uncurry setEnv)

testEnv :: [(String, String)] -> Expectation
testEnv expected = getEnvironment >>= (`shouldBe` sort expected) . sort

-- Test cases

richEnv1 :: RichEnv
richEnv1 = S.fromList [EnvVarValue (VarValue "SOME" "var")]

expectedEnv1 :: [(String, String)]
expectedEnv1 = [("SOME", "var")]

richEnv2 :: RichEnv
richEnv2 = S.fromList [EnvVarValue (VarValue "SOME" "var"), EnvVarValue (VarValue "OTHER" "othervar")]

expectedEnv2 :: [(String, String)]
expectedEnv2 = [("SOME", "var"), ("OTHER", "othervar")]

exampleEnv :: [(String, String)]
exampleEnv = [("FOO", "bar"), ("BAZ", "qux")]

richEnv3 :: RichEnv
richEnv3 = S.fromList [EnvVarNameMap (VarMap "FOO" "SOME")]

expectedEnv3 :: [(String, String)]
expectedEnv3 = [("SOME", "bar"), ("BAZ", "qux")]
