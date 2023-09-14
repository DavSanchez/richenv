{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types (Environment, NoWildcardNonEmptyString, NoWildcardString, mkNoWildcardNonEmptyString, mkNoWildcardString, UnwrapString (..)) where

import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)

type Environment = [(String, String)]

class (Show a) => UnwrapString a where
  unwrapString :: a -> String

newtype NoWildcardNonEmptyString = NoWildcardNonEmptyString (NE.NonEmpty Char)
  deriving stock (Eq, Show, Generic)

mkNoWildcardNonEmptyString :: String -> Maybe NoWildcardNonEmptyString
mkNoWildcardNonEmptyString s
  | '*' `elem` s = Nothing
  | otherwise = NoWildcardNonEmptyString <$> NE.nonEmpty s

instance UnwrapString NoWildcardNonEmptyString where
  unwrapString :: NoWildcardNonEmptyString -> String
  unwrapString (NoWildcardNonEmptyString s) = NE.toList s

instance Semigroup NoWildcardNonEmptyString where
  (<>) :: NoWildcardNonEmptyString -> NoWildcardNonEmptyString -> NoWildcardNonEmptyString
  (<>) (NoWildcardNonEmptyString a) (NoWildcardNonEmptyString b) = NoWildcardNonEmptyString $ a <> b

-- newtype WildcardString = WildcardString String
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (Hashable)

-- mkWildcardString :: String -> Maybe WildcardString
-- mkWildcardString s
--   | ((<$>) NE.last . NE.nonEmpty) s == Just '*' = WildcardString . NE.init <$> NE.nonEmpty s
--   | otherwise = Nothing

-- instance UnwrapString WildcardString where
--   unwrapString :: WildcardString -> String
--   unwrapString (WildcardString s) = s

-- instance Semigroup WildcardString where
--   (<>) :: WildcardString -> WildcardString -> WildcardString
--   (<>) (WildcardString a) (WildcardString b) = WildcardString $ a <> b

newtype NoWildcardString = NoWildcardString String
  deriving stock (Eq, Show, Generic)

mkNoWildcardString :: String -> Maybe NoWildcardString
mkNoWildcardString s
  | '*' `elem` s = Nothing
  | otherwise = Just $ NoWildcardString s

-- NoWildcardString can be empty, so we also define Monoid instances for it.

instance UnwrapString NoWildcardString where
  unwrapString :: NoWildcardString -> String
  unwrapString (NoWildcardString s) = s

instance Semigroup NoWildcardString where
  (<>) :: NoWildcardString -> NoWildcardString -> NoWildcardString
  (<>) (NoWildcardString a) (NoWildcardString b) = NoWildcardString $ a <> b

instance Monoid NoWildcardString where
  mempty :: NoWildcardString
  mempty = NoWildcardString mempty
