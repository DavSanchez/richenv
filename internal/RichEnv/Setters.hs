module RichEnv.Setters (setPrefixedVars, setVarMapValues, setVarValueEnv, varValuesToEnvironment) where

import Control.Monad (unless)
import Data.Bifunctor (bimap, first)
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import RichEnv.Types (Environment, UnwrapString (unwrapString))
import RichEnv.Types.VarMap (VarMap (..))
import RichEnv.Types.VarPrefix (VarPrefix (..))
import RichEnv.Types.VarValue (VarValue (..), mkVarValue)
import System.Environment (setEnv)

-- | Takes 'VarValue's and sets them as environment variables. It is a no-op if the variable name is empty.
setVarValueEnv :: VarValue -> IO ()
setVarValueEnv vv = do
  let name = unwrapString $ vvName vv
      value = unwrapString $ vvValue vv
  unless (null name) $ setEnv name value

varValuesToEnvironment :: HashSet VarValue -> Environment
varValuesToEnvironment = fmap toTuple . S.toList
  where
    toTuple vv = bimap unwrapString unwrapString (vvName vv, vvValue vv)

-- | Takes an environment list and all the 'VarMap's and prepares a valid @HashSet@ of 'VarValue's according to the RichEnv rules.
setVarMapValues :: Environment -> HashSet VarMap -> HashSet VarValue
setVarMapValues cEnv = foldr setVarMapValue mempty
  where
    setVarMapValue vm = do
      let name = unwrapString $ vmName vm
          from = unwrapString $ vmFrom vm
          value = lookup from cEnv
      case value of
        Just v -> maybe id S.insert $ mkVarValue name v
        Nothing -> id

-- | Takes an environment list and all the 'VarPrefix'es and prepares a @HashSet@ of 'VarValue's according to the RichEnv rules.
setPrefixedVars :: Environment -> HashSet VarPrefix -> HashSet VarValue
setPrefixedVars cEnv = foldr setPrefixedVar mempty
  where
    setPrefixedVar vp = do
      let newPrefix = unwrapString $ vpName vp
          oldPrefix = unwrapString $ vpFrom vp
          varsWithoutPrefix = mapMaybe (getWithoutPrefix oldPrefix) cEnv
          newPrefixedVars = fmap (first (newPrefix <>)) varsWithoutPrefix
          varValues = mapMaybe (uncurry mkVarValue) newPrefixedVars
      S.union $ S.fromList varValues
    getWithoutPrefix old (k, v) = stripPrefix old k >>= \sk -> pure (sk, v)
