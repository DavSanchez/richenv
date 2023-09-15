module RichEnv.Setters (mappingsToValues, prefixesToValues, valuesToEnv, valuesToEnvList, richEnvToValues) where

import Data.Bifunctor (first)
import Data.HashMap.Lazy qualified as HM
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import RichEnv.Types (Environment)
import RichEnv.Types.Mappings (Mappings (Mappings, unMappings))
import RichEnv.Types.Prefixes (Prefixes (Prefixes, unPrefixes))
import RichEnv.Types.RichEnv (RichEnv (..))
import RichEnv.Types.Values (Values (Values, unValues))
import System.Environment (setEnv)

valuesToEnv :: Values -> IO ()
valuesToEnv = mapM_ (uncurry setEnv) . HM.toList . unValues

valuesToEnvList :: Values -> Environment
valuesToEnvList = HM.toList . unValues

-- | Takes an environment list and all the value mappings and prepares a set of environment variables according to the RichEnv rules.
mappingsToValues :: Environment -> Mappings -> Values
mappingsToValues _ (Mappings m) | null m = mempty
mappingsToValues currentEnv m =
  let mappings' = HM.toList $ unMappings m
      value from = lookup from currentEnv
      setMappingValue (_, Nothing) = id
      setMappingValue (k, Just v) = HM.insert k v
   in Values $ foldr (setMappingValue . fmap value) mempty mappings'

-- | Takes an environment list and all the prefix mappings and prepares a set of environment variables according to the RichEnv rules.
prefixesToValues :: Environment -> Prefixes -> Values
prefixesToValues _ (Prefixes p) | null p = mempty
prefixesToValues currentEnv p =
  let prefixes' = HM.toList $ unPrefixes p
      res = if null prefixes' then [currentEnv] else fmap (setNewPrefix currentEnv) prefixes'
   in toValues $ mconcat res

setNewPrefix :: Environment -> (String, [String]) -> Environment
setNewPrefix currentEnv (newPrefix, []) = fmap (first (newPrefix <>)) currentEnv
setNewPrefix currentEnv (newPrefix, [""]) = fmap (first (newPrefix <>)) currentEnv
setNewPrefix currentEnv (newPrefix, oldPrefixes) =
  let varsWithoutPrefixes = removePrefix currentEnv <$> oldPrefixes
      newPrefixedVars = (fmap . fmap) (first (newPrefix <>)) varsWithoutPrefixes
   in mconcat newPrefixedVars

removePrefix :: Environment -> String -> Environment
removePrefix currentEnv oldPrefix =
  let getWithoutPrefix old (k, v) = stripPrefix old k >>= \sk -> pure (sk, v)
   in mapMaybe (getWithoutPrefix oldPrefix) currentEnv

toValues :: Environment -> Values
toValues = Values . HM.fromList

richEnvToValues :: Environment -> RichEnv -> Values
richEnvToValues currentEnv re =
  let vvs = values re
      vms = flip mappingsToValues (mappings re)
      vps = flip prefixesToValues (prefixes re)
   in vvs <> vms currentEnv <> vps currentEnv
