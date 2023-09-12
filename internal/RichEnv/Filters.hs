module RichEnv.Filters (varMaps, varPrefixes, varValues) where

import Control.Arrow ((>>>))
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import RichEnv.Types.RichEnv (RichEnv (richEnv), RichEnvItem (EnvVarNameMap, EnvVarPrefix, EnvVarValue))
import RichEnv.Types.VarMap (VarMap)
import RichEnv.Types.VarPrefix (VarPrefix)
import RichEnv.Types.VarValue (VarValue)

-- | Gets only the 'VarMap' items from a 'RichEnv'.
--
-- >>> varValues mempty == mempty
-- True
-- >>> import Data.List.NonEmpty (fromList)
-- >>> let richEnv = RichEnv $ S.fromList [EnvVarValue (VarValue (fromList "foo") "bar"), EnvVarNameMap (VarMap (fromList "bar") (fromList "baz")), EnvVarPrefix (VarPrefix "qux" "quux")]
-- >>> varValues richEnv == S.fromList [VarValue (fromList "foo") "bar"]
varMaps :: RichEnv -> HashSet VarMap
varMaps = richEnv >>> foldr f mempty
  where
    f (EnvVarNameMap vm) = S.insert vm
    f _ = id

-- | Gets only the 'VarValue' items from a 'RichEnv'.
--
-- >>> varValues mempty == mempty
-- True
-- >>> import Data.List.NonEmpty (fromList)
-- >>> let richEnv = RichEnv $ S.fromList [EnvVarValue (VarValue (fromList "foo") "bar"), EnvVarNameMap (VarMap (fromList "bar") (fromList "baz")), EnvVarPrefix (VarPrefix "qux" "quux")]
-- >>> varValues richEnv == S.fromList [VarValue (fromList "foo") "bar"]
-- True
varValues :: RichEnv -> HashSet VarValue
varValues = richEnv >>> foldr f mempty
  where
    f (EnvVarValue vv) = S.insert vv
    f _ = id

-- | Gets only the 'VarPrefix' items from a 'RichEnv'.
--
-- >>> varPrefixes mempty == mempty
-- True
-- >>> import Data.List.NonEmpty (fromList)
-- >>> let richEnv = RichEnv $ S.fromList [EnvVarValue (VarValue (fromList "foo") "bar"), EnvVarNameMap (VarMap (fromList "bar") (fromList "baz")), EnvVarPrefix (VarPrefix "qux" "quux")]
-- >>> varPrefixes richEnv == S.fromList [VarPrefix "qux" "quux"]
-- True
varPrefixes :: RichEnv -> HashSet VarPrefix
varPrefixes = richEnv >>> foldr f mempty
  where
    f (EnvVarPrefix vp) = S.insert vp
    f _ = id
