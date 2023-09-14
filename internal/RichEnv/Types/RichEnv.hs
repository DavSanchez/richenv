{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.RichEnv (RichEnv (RichEnv, values, mappings, prefixes), Values (Values, unValues), Mappings (Mappings, unMappings), Prefixes (Prefixes, unPrefixes)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict qualified as HM
import GHC.Generics (Generic)

data RichEnv = RichEnv
  { values :: Values,
    mappings :: Mappings,
    prefixes :: Prefixes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup RichEnv where
  (<>) :: RichEnv -> RichEnv -> RichEnv
  (<>) (RichEnv a b c) (RichEnv d e f) = RichEnv (a <> d) (b <> e) (c <> f)

instance Monoid RichEnv where
  mempty :: RichEnv
  mempty = RichEnv mempty mempty mempty

newtype Values = Values {unValues :: HM.HashMap String String}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup Values where
  (<>) :: Values -> Values -> Values
  (<>) (Values a) (Values b) = Values (a <> b)

instance Monoid Values where
  mempty :: Values
  mempty = Values mempty

newtype Mappings = Mappings {unMappings :: HM.HashMap String String}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup Mappings where
  (<>) :: Mappings -> Mappings -> Mappings
  (<>) (Mappings a) (Mappings b) = Mappings (a <> b)

instance Monoid Mappings where
  mempty :: Mappings
  mempty = Mappings mempty

newtype Prefixes = Prefixes {unPrefixes :: HM.HashMap String [String]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup Prefixes where
  (<>) :: Prefixes -> Prefixes -> Prefixes
  (<>) (Prefixes a) (Prefixes b) = Prefixes (a <> b)

instance Monoid Prefixes where
  mempty :: Prefixes
  mempty = Prefixes mempty
