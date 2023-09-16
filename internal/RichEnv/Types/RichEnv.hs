module RichEnv.Types.RichEnv (RichEnv (RichEnv, values, mappings, prefixes), Values, Mappings, Prefixes) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import RichEnv.Types.Mappings (Mappings)
import RichEnv.Types.Prefixes (Prefixes)
import RichEnv.Types.Values (Values)

data RichEnv = RichEnv
  { values :: Values,
    mappings :: Mappings,
    prefixes :: Prefixes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup RichEnv where
  (<>) :: RichEnv -> RichEnv -> RichEnv
  (<>) (RichEnv a b c) (RichEnv d e f) = RichEnv (a <> d) (b <> e) (c <> f)

instance Monoid RichEnv where
  mempty :: RichEnv
  mempty = RichEnv mempty mempty mempty
