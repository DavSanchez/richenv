module RichEnv.Types (RichEnv, RichEnvItem (..), VarMap (..), VarPrefix (..), VarValue (..), Environment) where

import Data.Set (Set)
import Data.Text (Text)

type RichEnv = Set RichEnvItem

type Environment = [(Text, Text)]

data RichEnvItem
  = -- | Maps an environment variable name to a different one.
    EnvVarNameMap VarMap
  | -- | Sets an environment variable to a specific value.
    EnvVarValue VarValue
  | -- | Maps all environment variables with a certain prefix to a new set of environment variables with a different prefix.
    EnvVarPrefix VarPrefix
  deriving stock (Eq, Ord, Show)

-- | A mapping from one environment variable name to another.
data VarMap = MkVarMap
  { -- | The name of the output environment variable.
    mapVarName :: Text,
    -- | The name of the input environment variable.
    mapVarFrom :: Text
  }
  deriving stock (Eq, Ord, Show)

data VarValue = MkVarValue
  { -- | The name of the environment variable.
    valueName :: Text,
    -- | The value of the environment variable.
    valueValue :: Text
  }
  deriving stock (Eq, Ord, Show)

-- | A prefix to add to all environment variables.
data VarPrefix = MkVarPrefix
  { -- | The prefix of the output environment. Can be empty or the same as @prefixFrom@.
    prefixName :: Text,
    -- | The prefix of the input environment.
    prefixFrom :: Text
  }
  deriving stock (Eq, Ord, Show)
