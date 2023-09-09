{-# LANGUAGE DeriveGeneric #-}

module RichEnv.Types (RichEnvItem (..), VarMap (..), VarPrefix (..), VarValue (..), RichEnv, Environment, NonEmptyString) where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
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

-- | A mapping from one environment variable name to another.
data VarMap = VarMap
  { -- | The name of the output environment variable.
    vmName :: NonEmptyString,
    -- | The name of the input environment variable.
    vmFrom :: NonEmptyString
  }
  deriving stock (Eq, Show, Generic)

data VarValue = VarValue
  { -- | The name of the environment variable.
    vvName :: NonEmptyString,
    -- | The value of the environment variable.
    vvValue :: String
  }
  deriving stock (Eq, Show, Generic)

-- | A prefix to add to all environment variables.
data VarPrefix = VarPrefix
  { -- | The prefix of the output environment. Can be empty (representing /all/ or the wildcard '*') or even the same as @prefixFrom@ to act as a passthrough.
    vpName :: String,
    -- | The prefix of the input environment.
    vpFrom :: String
  }
  deriving stock (Eq, Show, Generic)

-- Hashable instances for using HashSet. Requires instances of Generic.

instance Hashable RichEnvItem

instance Hashable VarMap

instance Hashable VarValue

instance Hashable VarPrefix
