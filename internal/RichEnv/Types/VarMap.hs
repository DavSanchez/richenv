{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.VarMap (VarMap (..)) where

import Control.Monad (when)
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value, object, pairs, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import RichEnv.Types (NonEmptyString)

-- | A mapping from one environment variable name to another.
data VarMap = VarMap
  { -- | The name of the output environment variable.
    vmName :: NonEmptyString,
    -- | The name of the input environment variable.
    vmFrom :: NonEmptyString
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance FromJSON VarMap where
  parseJSON :: Value -> Parser VarMap
  parseJSON = withObject "VarMap" $ \o -> do
    nameM <- NE.nonEmpty <$> o .: "name"
    fromM <- NE.nonEmpty <$> o .: "from"
    case (nameM, fromM) of
      (Just name, Just from) -> do
        when ('*' `elem` name || '*' `elem` from) $ fail "VarMap `name` or `from` cannot contain `*`"
        pure $ VarMap name from
      _ -> fail "VarMap must have fields `name` and `from`"

instance ToJSON VarMap where
  toJSON :: VarMap -> Value
  toJSON (VarMap n f) = object ["name" .= NE.toList n, "from" .= NE.toList f]

  toEncoding :: VarMap -> Encoding
  toEncoding (VarMap n f) = pairs ("name" .= NE.toList n <> "from" .= NE.toList f)
