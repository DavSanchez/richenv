{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.VarValue (VarValue (..)) where

import Control.Exception (Exception (displayException))
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value, object, pairs, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import RichEnv.Types (NonEmptyString)
import RichEnv.Types.ParseError (RichEnvParseError (..))

data VarValue = VarValue
  { -- | The name of the environment variable.
    vvName :: NonEmptyString,
    -- | The value of the environment variable.
    vvValue :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance FromJSON VarValue where
  parseJSON :: Value -> Parser VarValue
  parseJSON = withObject "VarValue" $ \o -> do
    nameM <- NE.nonEmpty <$> o .: "name"
    value <- o .: "value"
    case nameM of
      Just name -> pure $ VarValue name value
      _ -> fail $ displayException VarValueNoName

instance ToJSON VarValue where
  toJSON :: VarValue -> Value
  toJSON (VarValue n v) = object ["name" .= NE.toList n, "value" .= v]

  toEncoding :: VarValue -> Encoding
  toEncoding (VarValue n v) = pairs ("name" .= NE.toList n <> "value" .= v)
