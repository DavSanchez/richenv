{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryInstances () where

import Data.HashMap.Strict qualified as HM
import RichEnv.Types.RichEnv (Mappings (..), Prefixes (..), RichEnv (..), Values (..))
import Test.QuickCheck (Arbitrary (arbitrary), Gen)

instance Arbitrary RichEnv where
  arbitrary :: Gen RichEnv
  arbitrary = RichEnv <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Values where
  arbitrary :: Gen Values
  arbitrary = Values . HM.fromList <$> arbitrary

instance Arbitrary Mappings where
  arbitrary :: Gen Mappings
  arbitrary = Mappings . HM.fromList <$> arbitrary

instance Arbitrary Prefixes where
  arbitrary :: Gen Prefixes
  arbitrary = Prefixes . HM.fromList <$> arbitrary

-- instance (Arbitrary a, Hashable a) => Arbitrary (S.HashSet a) where
--   arbitrary :: Gen (S.HashSet a)
--   arbitrary = S.fromList <$> arbitrary

-- instance Arbitrary RichEnvItem where
--   arbitrary :: Gen RichEnvItem
--   arbitrary = oneof [EnvVarValue <$> arbitrary, EnvVarNameMap <$> arbitrary, EnvVarPrefix <$> arbitrary]

-- instance Arbitrary VarPrefix where
--   arbitrary :: Gen VarPrefix
--   arbitrary = do
--     pn <- unwrapString <$> (arbitrary :: Gen NoWildcardString)
--     pf <- unwrapString <$> (arbitrary :: Gen NoWildcardString)
--     pure $ fromJust (mkVarPrefix pn pf)

-- instance Arbitrary VarMap where
--   arbitrary :: Gen VarMap
--   arbitrary = do
--     mn <- unwrapString <$> (arbitrary :: Gen NoWildcardNonEmptyString)
--     mf <- unwrapString <$> (arbitrary :: Gen NoWildcardNonEmptyString)
--     pure $ fromJust (mkVarMap mn mf)

-- instance Arbitrary VarValue where
--   arbitrary :: Gen VarValue
--   arbitrary = do
--     vn <- unwrapString <$> (arbitrary :: Gen NoWildcardNonEmptyString)
--     vv <- filter (/= '*') <$> arbitrary
--     pure $ fromJust (mkVarValue vn vv)

-- instance (Arbitrary a) => Arbitrary (NE.NonEmpty a) where
--   arbitrary :: Gen (NE.NonEmpty a)
--   arbitrary = do
--     c <- arbitrary
--     cs <- arbitrary
--     pure (c :| cs)

-- instance Arbitrary NoWildcardNonEmptyString where
--   arbitrary :: Gen NoWildcardNonEmptyString
--   arbitrary = do
--     c <- oneof $ pure <$> allowedCharList
--     cs <- listOf $ elements allowedCharList
--     let res = fromJust $ mkNoWildcardNonEmptyString (c : cs) -- `fromJust` is safe here because we're only generating allowed characters.
--     pure res
--     where
--       allowedCharList = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_']

-- instance Arbitrary NoWildcardString where
--   arbitrary :: Gen NoWildcardString
--   arbitrary = do
--     cs <- listOf $ elements allowedCharList
--     let res = fromMaybe mempty $ mkNoWildcardString cs
--     pure res
--     where
--       allowedCharList = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_']
