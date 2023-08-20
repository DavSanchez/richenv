module RichEnv.Setters (setPrefixedVars, setVarMapValues, setVarValueEnv) where

import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (stripPrefix, unpack)
import RichEnv.Types (Environment, VarMap (..), VarPrefix (..), VarValue (..))
import System.Environment (setEnv)

-- | Takes all the 'VarValue's and sets them as environment variables.
setVarValueEnv :: VarValue -> IO ()
setVarValueEnv vv =
  let name = valueName vv
      value = valueValue vv
   in setEnv (unpack name) (unpack value)

setVarMapValues :: Environment -> Set VarMap -> Set VarValue
setVarMapValues cEnv = foldr setVarMapValue mempty
  where
    setVarMapValue vm = do
      let name = mapVarName vm
          from = mapVarFrom vm
          value = lookup from cEnv
      case value of
        Just v -> S.insert (VarValue name v)
        Nothing -> id

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
      S.union $ S.fromList $ fmap (uncurry VarValue) newPrefixedVars
