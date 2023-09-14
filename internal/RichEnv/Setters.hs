module RichEnv.Setters (mappingsToValues, prefixesToValues, valuesToEnv, valuesToEnvList) where

import Data.Bifunctor (first)
import Data.HashMap.Lazy qualified as HM
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import RichEnv.Types (Environment)
import RichEnv.Types.RichEnv (Mappings (unMappings), Prefixes (unPrefixes), Values (Values, unValues))
import System.Environment (setEnv)

-- -- | Takes 'VarValue's and sets them as environment variables. It is a no-op if the variable name is empty.
-- setVarValueEnv :: VarValue -> IO ()
-- setVarValueEnv vv = do
--   let name = unwrapString $ vvName vv
--       value = unwrapString $ vvValue vv
--   unless (null name) $ setEnv name value

-- varValuesToEnvironment :: HashSet VarValue -> Environment
-- varValuesToEnvironment = fmap toTuple . S.toList
--   where
--     toTuple vv = bimap unwrapString unwrapString (vvName vv, vvValue vv)

valuesToEnv :: Values -> IO ()
valuesToEnv = mapM_ (uncurry setEnv) . HM.toList . unValues

valuesToEnvList :: Values -> Environment
valuesToEnvList = HM.toList . unValues

-- | Takes an environment list and all the value mappings and prepares a set of environment variables according to the RichEnv rules.
mappingsToValues :: Environment -> Mappings -> Values
mappingsToValues currentEnv m =
  let mappings = HM.toList $ unMappings m
      value from = lookup from currentEnv
      setMappingValue (_, Nothing) = id
      setMappingValue (k, Just v) = HM.insert k v
   in Values $ foldr (setMappingValue . fmap value) mempty mappings

-- | Takes an environment list and all the prefix mappings and prepares a set of environment variables according to the RichEnv rules.
prefixesToValues :: Environment -> Prefixes -> Values
prefixesToValues currentEnv p =
  let prefixes = HM.toList $ unPrefixes p
      res = fmap (setNewPrefix currentEnv) prefixes
   in toValues $ mconcat res

setNewPrefix :: Environment -> (String, [String]) -> Environment
setNewPrefix currentEnv (_, []) = currentEnv
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

-- setVarMapValues :: Environment -> HashSet VarMap -> HashSet VarValue
-- setVarMapValues cEnv = foldr setVarMapValue mempty
--   where
--     setVarMapValue vm = do
--       let name = unwrapString $ vmName vm
--           from = unwrapString $ vmFrom vm
--           value = lookup from cEnv
--       case value of
--         Just v -> maybe id S.insert $ mkVarValue name v
--         Nothing -> id

-- | Takes an environment list and all the 'VarPrefix'es and prepares a @HashSet@ of 'VarValue's according to the RichEnv rules.
-- setPrefixedVars :: Environment -> HashSet VarPrefix -> HashSet VarValue
-- setPrefixedVars cEnv = foldr setPrefixedVar mempty
--   where
--     setPrefixedVar vp = do
--       let newPrefix = unwrapString $ vpName vp
--           oldPrefix = unwrapString $ vpFrom vp
--           varsWithoutPrefix = mapMaybe (getWithoutPrefix oldPrefix) cEnv
--           newPrefixedVars = fmap (first (newPrefix <>)) varsWithoutPrefix
--           varValues = mapMaybe (uncurry mkVarValue) newPrefixedVars
--       S.union $ S.fromList varValues
--     getWithoutPrefix old (k, v) = stripPrefix old k >>= \sk -> pure (sk, v)
