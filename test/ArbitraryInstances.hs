{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryInstances () where

import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, fromMaybe)
import RichEnv.Types (NoWildcardNonEmptyString, NoWildcardString, UnwrapString (unwrapString), mkNoWildcardNonEmptyString, mkNoWildcardString)
import RichEnv.Types.RichEnv (RichEnv (..), RichEnvItem (..))
import RichEnv.Types.VarMap (VarMap (..), mkVarMap)
import RichEnv.Types.VarPrefix (VarPrefix (..), mkVarPrefix)
import RichEnv.Types.VarValue (VarValue (..), mkVarValue)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements, listOf, oneof)

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
    pn <- unwrapString <$> (arbitrary :: Gen NoWildcardString)
    pf <- unwrapString <$> (arbitrary :: Gen NoWildcardString)
    pure $ fromJust (mkVarPrefix pn pf)

instance Arbitrary VarMap where
  arbitrary :: Gen VarMap
  arbitrary = do
    mn <- unwrapString <$> (arbitrary :: Gen NoWildcardNonEmptyString)
    mf <- unwrapString <$> (arbitrary :: Gen NoWildcardNonEmptyString)
    pure $ fromJust (mkVarMap mn mf)

instance Arbitrary VarValue where
  arbitrary :: Gen VarValue
  arbitrary = do
    vn <- unwrapString <$> (arbitrary :: Gen NoWildcardNonEmptyString)
    vv <- filter (/= '*') <$> arbitrary
    pure $ fromJust (mkVarValue vn vv)

instance (Arbitrary a) => Arbitrary (NE.NonEmpty a) where
  arbitrary :: Gen (NE.NonEmpty a)
  arbitrary = do
    c <- arbitrary
    cs <- arbitrary
    pure (c :| cs)

instance Arbitrary NoWildcardNonEmptyString where
  arbitrary :: Gen NoWildcardNonEmptyString
  arbitrary = do
    c <- oneof $ pure <$> allowedCharList
    cs <- listOf $ elements allowedCharList
    let res = fromJust $ mkNoWildcardNonEmptyString (c : cs) -- `fromJust` is safe here because we're only generating allowed characters.
    pure res
    where
      allowedCharList = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_']

instance Arbitrary NoWildcardString where
  arbitrary :: Gen NoWildcardString
  arbitrary = do
    cs <- listOf $ elements allowedCharList
    let res = fromMaybe mempty $ mkNoWildcardString cs
    pure res
    where
      allowedCharList = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_']
