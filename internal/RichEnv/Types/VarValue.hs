{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.VarValue (VarValue (vvName, vvValue), mkVarValue) where

import Control.Exception (Exception (displayException))
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value, object, pairs, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import RichEnv.Types (NoWildcardNonEmptyString, NoWildcardString, UnwrapString (unwrapString), mkNoWildcardNonEmptyString, mkNoWildcardString)
import RichEnv.Types.ParseError (RichEnvParseError (..))

data VarValue = VarValue
  { -- | The name of the environment variable.
    vvName :: NoWildcardNonEmptyString,
    -- | The value of the environment variable.
    vvValue :: NoWildcardString
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

mkVarValue :: String -> String -> Maybe VarValue
mkVarValue n v = VarValue <$> mkNoWildcardNonEmptyString n <*> mkNoWildcardString v

instance FromJSON VarValue where
  parseJSON :: Value -> Parser VarValue
  parseJSON = withObject "VarValue" $ \o -> do
    nameV <- o .: "name"
    valueV <- o .: "value"
    case (nameV, valueV) of
      (Just n, Just v) -> do
        name <- maybe (fail $ displayException VarMapWildcards) pure $ mkNoWildcardNonEmptyString n
        value <- maybe (fail $ displayException VarMapWildcards) pure $ mkNoWildcardString v
        pure $ VarValue name value
      (Just n, Nothing) -> do
        name <- maybe (fail $ displayException VarMapWildcards) pure $ mkNoWildcardNonEmptyString n
        pure $ VarValue name mempty
      (Nothing, _) -> fail $ displayException VarValueNoName

instance ToJSON VarValue where
  toJSON :: VarValue -> Value
  toJSON (VarValue n v) = object ["name" .= unwrapString n, "value" .= unwrapString v]

  toEncoding :: VarValue -> Encoding
  toEncoding (VarValue n v) = pairs ("name" .= unwrapString n <> "value" .= unwrapString v)
