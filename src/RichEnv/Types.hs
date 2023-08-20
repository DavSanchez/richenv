module RichEnv.Types (RichEnv, RichEnvItem (..), VarMap (..), VarPrefix (..), VarValue (..), Environment) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

type RichEnv = Set RichEnvItem

type Environment = [(Text, Text)] -- Map Text Text

data RichEnvItem
  = -- | Maps an environment variable name to a different one.
    EnvVarNameMap VarMap
  | -- | Sets an environment variable to a specific value.
    EnvVarValue VarValue
  | -- | Maps all environment variables with a certain prefix to a new set of environment variables with a different prefix.
    EnvVarPrefix VarPrefix

-- | A mapping from one environment variable name to another.
data VarMap = VarMap
  { -- | The name of the output environment variable.
    mapVarName :: Text,
    -- | The name of the input environment variable.
    mapVarFrom :: Text
  }
  deriving stock (Eq, Ord)

data VarValue = VarValue
  { -- | The name of the environment variable.
    valueName :: Text,
    -- | The value of the environment variable.
    valueValue :: Text
  }
  deriving stock (Eq, Ord)

-- | A prefix to add to all environment variables.
data VarPrefix = VarPrefix
  { -- | The prefix of the output environment. Can be empty or the same as @prefixFrom@.
    prefixName :: Text,
    -- | The prefix of the input environment.
    prefixFrom :: Text
  }
  deriving stock (Eq, Ord)
