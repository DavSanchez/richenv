module RichEnv.Filters (varMaps, varPrefixes, varValues) where

import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import RichEnv.Types (RichEnv, RichEnvItem (..), VarMap (..), VarPrefix (..), VarValue (..))

-- | Gets only the 'VarMap' items from a 'RichEnv'.
--
-- >>> varValues S.empty == S.empty
-- True
-- >>> let richEnv = S.fromList [EnvVarValue (MkVarValue "foo" "bar"), EnvVarNameMap (MkVarMap "bar" "baz"), EnvVarPrefix (MkVarPrefix "qux" "quux")]
-- >>> varValues richEnv == S.fromList [MkVarValue "foo" "bar"]
-- True
varMaps :: RichEnv -> HashSet VarMap
varMaps = S.foldr f S.empty
  where
    f (EnvVarNameMap vm) = S.insert vm
    f _ = id

-- | Gets only the 'VarValue' items from a 'RichEnv'.
--
-- >>> varValues S.empty == S.empty
-- True
-- >>> let richEnv = S.fromList [EnvVarValue (MkVarValue "foo" "bar"), EnvVarNameMap (MkVarMap "bar" "baz"), EnvVarPrefix (MkVarPrefix "qux" "quux")]
-- >>> varValues richEnv == S.fromList [MkVarValue "foo" "bar"]
-- True
varValues :: RichEnv -> HashSet VarValue
varValues = S.foldr f S.empty
  where
    f (EnvVarValue vv) = S.insert vv
    f _ = id

-- | Gets only the 'VarPrefix' items from a 'RichEnv'.
--
-- >>> varPrefixes S.empty == S.empty
-- True
-- >>> let richEnv = S.fromList [EnvVarValue (MkVarValue "foo" "bar"), EnvVarNameMap (MkVarMap "bar" "baz"), EnvVarPrefix (MkVarPrefix "qux" "quux")]
-- >>> varPrefixes richEnv == S.fromList [MkVarPrefix "qux" "quux"]
-- True
varPrefixes :: RichEnv -> HashSet VarPrefix
varPrefixes = S.foldr f S.empty
  where
    f (EnvVarPrefix vp) = S.insert vp
    f _ = id
