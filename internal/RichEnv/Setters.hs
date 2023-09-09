module RichEnv.Setters (setPrefixedVars, setVarMapValues, setVarValueEnv, varValuesToEnvironment) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.List (stripPrefix)
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (mapMaybe)
import RichEnv.Types (Environment, VarMap (..), VarPrefix (..), VarValue (..))
import System.Environment (setEnv)

-- | Takes 'VarValue's and sets them as environment variables. It is a no-op if the variable name is empty.
--
-- >>> import System.Environment
-- >>> value <- setVarValueEnv (VarValue "foo" "bar") >> getEnv "foo"
-- >>> value == "bar"
-- True
-- >>> import System.Environment
-- >>> getEnvironment >>= mapM_ (unsetEnv . fst)
-- >>> value <- setVarValueEnv (VarValue "foo" "bar") >> getEnv "foo"
-- >>> value == "bar"
-- True
-- >>> import System.Environment
-- >>> env <- getEnvironment
-- >>> setVarValueEnv (VarValue "" "bar")
-- >>> newEnv <- getEnvironment
-- >>> env == newEnv
-- True
setVarValueEnv :: VarValue -> IO ()
setVarValueEnv vv = do
  let name = toList $ vvName vv
      value = vvValue vv
  unless (null name) $ setEnv name value

varValuesToEnvironment :: HashSet VarValue -> Environment
varValuesToEnvironment = fmap toTuple . S.toList
  where
    toTuple vv = (toList (vvName vv), vvValue vv)

-- | Takes an environment list and all the 'VarMap's and prepares a valid @HashSet@ of 'VarValue's according to the RichEnv rules.
setVarMapValues :: Environment -> HashSet VarMap -> HashSet VarValue
setVarMapValues cEnv = foldr setVarMapValue mempty
  where
    setVarMapValue vm = do
      let name = vmName vm
          from = toList $ vmFrom vm
          value = lookup from cEnv
      case value of
        Just v -> S.insert (VarValue name v)
        Nothing -> id

-- | Takes an environment list and all the 'VarPrefix'es and prepares a @HashSet@ of 'VarValue's according to the RichEnv rules.
setPrefixedVars :: Environment -> HashSet VarPrefix -> HashSet VarValue
setPrefixedVars cEnv = foldr setPrefixedVar mempty
  where
    setPrefixedVar vp = do
      let newPrefix = vpName vp
          oldPrefix = vpFrom vp
          vars = mapMaybe (getWithoutPrefix oldPrefix) cEnv
          newPrefixedVars = fmap (first (newPrefix <>)) vars
          nonEmptyVarNames (n, v) = nonEmpty n >>= \nn -> pure (nn, v)
          withNonEmptyVarNames = mapMaybe nonEmptyVarNames newPrefixedVars
      S.union $ S.fromList $ fmap (uncurry VarValue) withNonEmptyVarNames
    getWithoutPrefix old (k, v) = stripPrefix old k >>= \sk -> pure (sk, v)
