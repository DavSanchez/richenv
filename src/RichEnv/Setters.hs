module RichEnv.Setters (setPrefixedVars, setVarMapValues, setVarValueEnv) where

import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (stripPrefix, unpack)
import RichEnv.Types (Environment, VarMap (..), VarPrefix (..), VarValue (..))
import System.Environment (setEnv)

-- | Takes all the 'VarValue's and sets them as environment variables.
-- >>> import System.Environment
-- >>> value <- setVarValueEnv (MkVarValue "foo" "bar") >> getEnv "foo"
-- >>> value == "bar"
-- True
-- >>> import System.Environment
-- >>> getEnvironment >>= mapM_ (unsetEnv . fst)
-- >>> value <- setVarValueEnv (MkVarValue "foo" "bar") >> getEnv "foo"
-- >>> value == "bar"
-- True
setVarValueEnv :: VarValue -> IO ()
setVarValueEnv vv =
  let name = valueName vv
      value = valueValue vv
   in setEnv (unpack name) (unpack value)

-- | Takes an environment list and all the 'VarMap's and prepares a valid @Set@ of 'VarValue's according to the RichEnv rules.
setVarMapValues :: Environment -> Set VarMap -> Set VarValue
setVarMapValues cEnv = foldr setVarMapValue mempty
  where
    setVarMapValue vm = do
      let name = mapVarName vm
          from = mapVarFrom vm
          value = lookup from cEnv
      case value of
        Just v -> S.insert (MkVarValue name v)
        Nothing -> id

-- | Takes an environment list and all the 'VarPrefix'es and prepares a @Set@ of 'VarValue's according to the RichEnv rules.
setPrefixedVars :: Environment -> Set VarPrefix -> Set VarValue
setPrefixedVars cEnv = foldr setPrefixedVar mempty
  where
    setPrefixedVar vp = do
      let prefix = prefixName vp
          from = prefixFrom vp
          vars =
            mapMaybe
              (\(k, v) -> stripPrefix from k >>= \sk -> Just (sk, v))
              cEnv
          newPrefixedVars = fmap (first (prefix <>)) vars
      S.union $ S.fromList $ fmap (uncurry MkVarValue) newPrefixedVars
