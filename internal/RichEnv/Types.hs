{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types (RichEnvItem (..), VarMap (..), VarPrefix (..), VarValue (..), RichEnv (..), Environment, NonEmptyString) where

import Control.Applicative ((<|>))
import Control.Monad (when, (>=>))
import Data.Aeson (Encoding, FromJSON (..), ToJSON (..), Value (..), object, pairs, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)

newtype RichEnv = RichEnv {richEnv :: S.HashSet RichEnvItem}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Semigroup RichEnv where
  (<>) :: RichEnv -> RichEnv -> RichEnv
  (<>) (RichEnv a) (RichEnv b) = RichEnv $ a <> b

instance Monoid RichEnv where
  mempty :: RichEnv
  mempty = RichEnv mempty

instance FromJSON RichEnv where
  parseJSON :: Value -> Parser RichEnv
  parseJSON = parseJSON >=> pure . RichEnv

instance ToJSON RichEnv where
  toJSON :: RichEnv -> Value
  toJSON (RichEnv env) = toJSON env

  toEncoding :: RichEnv -> Encoding
  toEncoding (RichEnv env) = toEncoding env

type Environment = [(String, String)]

type NonEmptyString = NE.NonEmpty Char

data RichEnvItem
  = -- | Maps an environment variable name to a different one.
    EnvVarNameMap VarMap
  | -- | Sets an environment variable to a specific value.
    EnvVarValue VarValue
  | -- | Maps all environment variables with a certain prefix to a new set of environment variables with a different prefix.
    EnvVarPrefix VarPrefix
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance FromJSON RichEnvItem where
  parseJSON :: Value -> Parser RichEnvItem
  parseJSON v = do
    let parseEnvVarNameMap = EnvVarNameMap <$> parseJSON v
        parseEnvVarValue = EnvVarValue <$> parseJSON v
        parseEnvVarPrefix = EnvVarPrefix <$> parseJSON v
    parseEnvVarNameMap <|> parseEnvVarValue <|> parseEnvVarPrefix

instance ToJSON RichEnvItem where
  toJSON :: RichEnvItem -> Value
  toJSON (EnvVarValue vv) = toJSON vv
  toJSON (EnvVarNameMap vm) = toJSON vm
  toJSON (EnvVarPrefix vp) = toJSON vp

  toEncoding :: RichEnvItem -> Encoding
  toEncoding (EnvVarValue vv) = toEncoding vv
  toEncoding (EnvVarNameMap vm) = toEncoding vm
  toEncoding (EnvVarPrefix vp) = toEncoding vp

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
      _ -> fail "VarValue must have field `name`"

instance ToJSON VarValue where
  toJSON :: VarValue -> Value
  toJSON (VarValue n v) = object ["name" .= NE.toList n, "value" .= v]

  toEncoding :: VarValue -> Encoding
  toEncoding (VarValue n v) = pairs ("name" .= NE.toList n <> "value" .= v)

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
