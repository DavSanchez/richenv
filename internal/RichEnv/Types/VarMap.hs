{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.VarMap (VarMap (vmName, vmFrom), mkVarMap) where

import Control.Exception (Exception (displayException))
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value, object, pairs, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import RichEnv.Types (NoWildcardNonEmptyString, UnwrapString (unwrapString), mkNoWildcardNonEmptyString)
import RichEnv.Types.ParseError (RichEnvParseError (..))

-- | A mapping from one environment variable name to another.
data VarMap = VarMap
  { -- | The name of the output environment variable.
    vmName :: NoWildcardNonEmptyString,
    -- | The name of the input environment variable.
    vmFrom :: NoWildcardNonEmptyString
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

mkVarMap :: String -> String -> Maybe VarMap
mkVarMap n f = VarMap <$> mkNoWildcardNonEmptyString n <*> mkNoWildcardNonEmptyString f

instance FromJSON VarMap where
  parseJSON :: Value -> Parser VarMap
  parseJSON = withObject "VarMap" $ \o -> do
    nameM <- o .: "name"
    fromM <- o .: "from"
    case (nameM, fromM) of
      (Just n, Just f) -> do
        name <- maybe (fail $ displayException VarMapWildcards) pure $ mkNoWildcardNonEmptyString n
        from <- maybe (fail $ displayException VarMapWildcards) pure $ mkNoWildcardNonEmptyString f
        pure $ VarMap name from
      _ -> fail $ displayException VarMapMissingFields

instance ToJSON VarMap where
  toJSON :: VarMap -> Value
  toJSON (VarMap n f) = object ["name" .= unwrapString n, "from" .= unwrapString f]

  toEncoding :: VarMap -> Encoding
  toEncoding (VarMap n f) = pairs ("name" .= unwrapString n <> "from" .= unwrapString f)
