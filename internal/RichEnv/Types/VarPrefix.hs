{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.VarPrefix (VarPrefix (vpName, vpFrom), mkVarPrefix) where

import Control.Exception (Exception (..))
import Control.Monad (when)
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toJSON), Value, object, pairs, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, ToJSON (toEncoding))
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import RichEnv.Types (NoWildcardString, UnwrapString (unwrapString), mkNoWildcardString)
import RichEnv.Types.ParseError (RichEnvParseError (..))

-- | A prefix to add to all environment variables.
data VarPrefix = VarPrefix
  { -- | The prefix of the output environment. Can be empty (representing /all/ or the wildcard '*') or even the same as @prefixFrom@ to act as a passthrough.
    vpName :: NoWildcardString,
    -- | The prefix of the input environment.
    vpFrom :: NoWildcardString
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

mkVarPrefix :: String -> String -> Maybe VarPrefix
mkVarPrefix n f = VarPrefix <$> mkNoWildcardString n <*> mkNoWildcardString f

instance FromJSON VarPrefix where
  parseJSON :: Value -> Parser VarPrefix
  parseJSON = withObject "VarPrefix" $ \o -> do
    nameP <- o .: "name"
    fromP <- o .: "from"
    -- Prefixes must have only one wildcard in each of the fields,
    -- and they must be at the end.
    case (NE.nonEmpty =<< nameP, NE.nonEmpty =<< fromP) of
      (Just n, Just f) -> do
        when (NE.last n /= '*' || NE.last f /= '*') $ fail $ displayException VarPrefixInvalidWildcards
        let vp = mkVarPrefix (NE.init n) (NE.init f)
        maybe (fail $ displayException VarPrefixInvalidWildcards) pure vp
      _ -> fail $ displayException VarPrefixMissingFields

instance ToJSON VarPrefix where
  toJSON :: VarPrefix -> Value
  toJSON (VarPrefix n f) = object ["name" .= (unwrapString n <> "*"), "from" .= (unwrapString f <> "*")]

  toEncoding :: VarPrefix -> Encoding
  toEncoding (VarPrefix n f) = pairs ("name" .= (unwrapString n <> "*") <> "from" .= (unwrapString f <> "*"))
