module RichEnv.Setters (mappingsToValues, prefixesToValues, valuesToEnv, valuesToEnvList, richEnvToValues) where

import Data.Bifunctor (first)
import Data.HashMap.Lazy qualified as HM
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import RichEnv.Types (Environment, fromEnvironment)
import RichEnv.Types.Mappings (Mappings (Mappings, unMappings))
import RichEnv.Types.Prefixes (Prefixes (Prefixes, unPrefixes))
import RichEnv.Types.RichEnv (RichEnv (..))
import RichEnv.Types.Values (Values (Values, unValues))
import System.Environment (setEnv)

valuesToEnv :: Values -> IO ()
valuesToEnv = mapM_ (uncurry setEnv) . fromEnvironment . HM.toList . unValues

valuesToEnvList :: Values -> Environment
valuesToEnvList = HM.toList . unValues

-- | Takes an environment list and all the value mappings and prepares a set of environment variables according to the RichEnv rules.
mappingsToValues :: Environment -> Mappings -> Values
mappingsToValues _ (Mappings m) | null m = mempty
mappingsToValues currentEnv m =
  let mappings' = unMappings m
      value from = lookup from currentEnv
      setMappingValue _ Nothing = id
      setMappingValue k (Just v) = HM.insert k v
      mappingsToValues' k v = setMappingValue k (value v)
   in Values $ HM.foldrWithKey' mappingsToValues' mempty mappings'

-- | Takes an environment list and all the prefix mappings and prepares a set of environment variables according to the RichEnv rules.
prefixesToValues :: Environment -> Prefixes -> Values
prefixesToValues _ (Prefixes p) | null p = mempty
prefixesToValues currentEnv p =
  let prefixes' = unPrefixes p
      prefixesToValues' k v env = env <> setNewPrefix k v currentEnv
      res = if null prefixes' then currentEnv else HM.foldrWithKey' prefixesToValues' mempty prefixes'
   in toValues res

setNewPrefix :: Text -> [Text] -> Environment -> Environment
setNewPrefix newPrefix [] currentEnv = fmap (first (newPrefix <>)) currentEnv
setNewPrefix newPrefix [""] currentEnv = fmap (first (newPrefix <>)) currentEnv
setNewPrefix newPrefix oldPrefixes currentEnv =
  let varsWithoutPrefixes = removePrefix currentEnv <$> oldPrefixes
      newPrefixedVars = (fmap . fmap) (first (newPrefix <>)) varsWithoutPrefixes
   in mconcat newPrefixedVars

removePrefix :: Environment -> Text -> Environment
removePrefix currentEnv oldPrefix =
  let getWithoutPrefix old (k, v) = T.stripPrefix old k >>= \sk -> pure (sk, v)
   in mapMaybe (getWithoutPrefix oldPrefix) currentEnv

toValues :: Environment -> Values
toValues = Values . HM.fromList

richEnvToValues :: Environment -> RichEnv -> Values
richEnvToValues currentEnv re =
  let vvs = values re
      vms = flip mappingsToValues (mappings re)
      vps = flip prefixesToValues (prefixes re)
   in vvs <> vms currentEnv <> vps currentEnv
