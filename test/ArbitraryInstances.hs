{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryInstances () where

import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import RichEnv.Types.RichEnv (RichEnv (..), RichEnvItem (..))
import RichEnv.Types.VarMap (VarMap (..))
import RichEnv.Types.VarPrefix (VarPrefix (..))
import RichEnv.Types.VarValue (VarValue (..))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, oneof)

instance Arbitrary RichEnv where
  arbitrary :: Gen RichEnv
  arbitrary = RichEnv <$> arbitrary

instance (Arbitrary a, Hashable a) => Arbitrary (S.HashSet a) where
  arbitrary :: Gen (S.HashSet a)
  arbitrary = S.fromList <$> arbitrary

instance Arbitrary RichEnvItem where
  arbitrary :: Gen RichEnvItem
  arbitrary = oneof [EnvVarValue <$> arbitrary, EnvVarNameMap <$> arbitrary, EnvVarPrefix <$> arbitrary]

instance Arbitrary VarPrefix where
  arbitrary :: Gen VarPrefix
  arbitrary = do
    name <- NE.filter ('*' /=) <$> arbitrary
    from <- NE.filter ('*' /=) <$> arbitrary
    pure $ VarPrefix name from

instance Arbitrary VarMap where
  arbitrary :: Gen VarMap
  arbitrary = do
    name <- ('m' :|) . NE.filter ('*' /=) <$> arbitrary
    from <- ('m' :|) . NE.filter ('*' /=) <$> arbitrary
    pure $ VarMap name from

instance Arbitrary VarValue where
  arbitrary :: Gen VarValue
  arbitrary = do
    name <- NE.filter ('*' /=) <$> arbitrary
    VarValue ('v' :| name) <$> arbitrary

instance (Arbitrary a) => Arbitrary (NE.NonEmpty a) where
  arbitrary :: Gen (NE.NonEmpty a)
  arbitrary = do
    c <- arbitrary
    cs <- arbitrary
    pure (c :| cs)
