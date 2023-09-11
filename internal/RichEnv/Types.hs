{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types (RichEnvItem (..), VarMap (..), VarPrefix (..), VarValue (..), RichEnv, Environment, NonEmptyString) where

import Data.Aeson (Encoding, FromJSON, ToJSON (..), Value, withObject, (.:), (.:?))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty, fromList)
import GHC.Generics (Generic)

type RichEnv = HashSet RichEnvItem

type Environment = [(String, String)]

type NonEmptyString = NonEmpty Char

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
  parseJSON = withObject "RichEnvItem" $ \o -> do
    name <- o .: "name"
    from <- o .:? "from"
    value <- o .:? "value"
    case (name, from, value) of
      (Just n, Nothing, Just v) -> pure $ EnvVarValue $ VarValue (fromList n) v
      (Just n, Just f, Nothing) -> do
        if '*' `notElem` n && '*' `notElem` f
          then pure $ EnvVarNameMap $ VarMap (fromList n) (fromList f)
          else do
            let n' = init n
                f' = init f
            if '*' `notElem` n' && '*' `notElem` f'
              then pure $ EnvVarPrefix $ VarPrefix n' f'
              else fail "VarMap `name` and `from` must end with a `*` and not contain `*` anywhere else."
      (Nothing, _, _) -> fail "RichEnvItem must have field `name`"
      (_, Nothing, Nothing) -> fail "RichEnvItem must have field `name` and at least one of `from` or `value`"
      _ -> fail "RichEnvItem must have only one of `from` or `value`"

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
  deriving anyclass (Hashable, ToJSON)

data VarValue = VarValue
  { -- | The name of the environment variable.
    vvName :: NonEmptyString,
    -- | The value of the environment variable.
    vvValue :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON)

-- | A prefix to add to all environment variables.
data VarPrefix = VarPrefix
  { -- | The prefix of the output environment. Can be empty (representing /all/ or the wildcard '*') or even the same as @prefixFrom@ to act as a passthrough.
    vpName :: String,
    -- | The prefix of the input environment.
    vpFrom :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON)
