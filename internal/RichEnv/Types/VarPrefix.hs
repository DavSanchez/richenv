{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.VarPrefix (VarPrefix (..)) where

import Control.Monad (when)
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toJSON), Value, object, pairs, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, ToJSON (toEncoding))
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import RichEnv.Types (NonEmptyString)

-- | A prefix to add to all environment variables.
data VarPrefix = VarPrefix
  { -- | The prefix of the output environment. Can be empty (representing /all/ or the wildcard '*') or even the same as @prefixFrom@ to act as a passthrough.
    vpName :: String,
    -- | The prefix of the input environment.
    vpFrom :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance FromJSON VarPrefix where
  parseJSON :: Value -> Parser VarPrefix
  parseJSON = withObject "VarPrefix" $ \o -> do
    name <- NE.nonEmpty <$> o .: "name"
    from <- NE.nonEmpty <$> o .: "from"
    -- Prefixes should have only one wildcard in each of the fields,
    -- and they should be at the end.
    case (name, from) of
      (Just name', Just from') -> do
        when (checkWildcard name' || checkWildcard from') $ fail "VarPrefix `name` and `from` must end with a `*` and not contain `*` anywhere else."
        pure $ VarPrefix ((init . NE.toList) name') ((init . NE.toList) from')
      _ -> fail "VarPrefix must have fields `name` and `from`"

instance ToJSON VarPrefix where
  toJSON :: VarPrefix -> Value
  toJSON (VarPrefix n f) = object ["name" .= (n <> "*"), "from" .= (f <> "*")]

  toEncoding :: VarPrefix -> Encoding
  toEncoding (VarPrefix n f) = pairs ("name" .= (n <> "*") <> "from" .= (f <> "*"))

checkWildcard :: NonEmptyString -> Bool
checkWildcard = elem '*' . NE.init
