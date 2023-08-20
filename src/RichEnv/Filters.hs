module RichEnv.Filters (varMaps, varPrefixes, varValues) where

import Data.Set (Set)
import Data.Set qualified as S
import RichEnv.Types (RichEnv, RichEnvItem (..), VarMap, VarPrefix, VarValue)

-- | Gets only the 'VarMap' items from a 'RichEnv'.
varMaps :: RichEnv -> Set VarMap
varMaps = S.foldr f S.empty
  where
    f (EnvVarNameMap vm) = S.insert vm
    f _ = id

-- | Gets only the 'VarValue' items from a 'RichEnv'.
varValues :: RichEnv -> Set VarValue
varValues = S.foldr f S.empty
  where
    f (EnvVarValue vv) = S.insert vv
    f _ = id

-- | Gets only the 'VarPrefix' items from a 'RichEnv'.
varPrefixes :: RichEnv -> Set VarPrefix
varPrefixes = S.foldr f S.empty
  where
    f (EnvVarPrefix vp) = S.insert vp
    f _ = id
