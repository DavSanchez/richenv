module RichEnv.Setters (setPrefixedVars, setVarMapValues, setVarValueEnv) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.Maybe (mapMaybe)
import Data.Text (stripPrefix, unpack)
import Data.Text qualified as T
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
  let name = vvName vv
      value = vvValue vv
  unless (T.null name) $ setEnv (unpack name) (unpack value)

-- | Takes an environment list and all the 'VarMap's and prepares a valid @HashSet@ of 'VarValue's according to the RichEnv rules.
setVarMapValues :: Environment -> HashSet VarMap -> HashSet VarValue
setVarMapValues cEnv = foldr setVarMapValue mempty
  where
    setVarMapValue (VarMap "" _) = id
    setVarMapValue vm = do
      let name = vmName vm
          from = vmFrom vm
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
      S.union $ S.fromList $ fmap (uncurry VarValue) newPrefixedVars
    getWithoutPrefix old (k, v) = stripPrefix old k >>= \sk -> pure (sk, v)
