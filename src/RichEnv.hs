module RichEnv (clearEnvironment, richEnv, RichEnv, VarMap, VarPrefix, VarValue) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Text (Text, pack, stripPrefix, unpack)
import RichEnv.Filters (varMaps, varPrefixes, varValues)
import RichEnv.Types (Environment, RichEnv, VarMap (..), VarPrefix (..), VarValue (..))
import System.Environment (getEnvironment, setEnv, unsetEnv)

{- | Clears all environment variables of the current process.

 >>> import System.Environment
 >>> clearEnvironment >> getEnvironment >>= pure . null
 True
 >>> import System.Environment
 >>> clearEnvironment >> setEnv "FOO" "bar" >> getEnvironment >>= \s -> pure (s == [("FOO", "bar")])
 True
-}
clearEnvironment :: IO ()
clearEnvironment = getEnvironment >>= mapM_ (unsetEnv . fst)

-- | Sets the environment variables available to the current process abiding to the 'RichEnv' rules.
richEnv :: RichEnv -> IO ()
richEnv re = do
    currentEnv <- M.fromList . texts <$> getEnvironment
    clearEnvironment
    setVarValues vvs
    setVarMapValues currentEnv vms
    setPrefixedVars currentEnv vps
    pure ()
  where
    vms = varMaps re
    vvs = varValues re
    vps = varPrefixes re

-- | Takes all the 'VarValue's and sets them as environment variables.
setVarValues :: Set VarValue -> IO ()
setVarValues = mapM_ setEnvFromItem
  where
    setEnvFromItem vv = do
        let name = valueName vv
            value = valueValue vv
        setEnv (unpack name) (unpack value)

setVarMapValues :: Environment -> Set VarMap -> IO ()
setVarMapValues cEnv = mapM_ setVarMapValue
  where
    setVarMapValue vm = do
        let name = mapVarName vm
            from = mapVarFrom vm
            value = M.lookup from cEnv
        case value of
            Just v -> setEnv (unpack name) (unpack v)
            Nothing -> pure ()

setPrefixedVars :: Environment -> Set VarPrefix -> IO ()
setPrefixedVars cEnv = mapM_ setPrefixedVar
  where
    setPrefixedVar vp = do
        let prefix = prefixName vp
            from = prefixFrom vp
            vars = M.mapMaybeWithKey (\k _ -> stripPrefix from k) cEnv
            newPrefixedVars = M.mapKeys (prefix <>) vars
        M.traverseWithKey setEnvFromItem newPrefixedVars
    setEnvFromItem k v = setEnv (unpack k) (unpack v)

{- AUXILIARY FUNCTIONS -}

-- | Converts the '[(String, String)]@ returned by 'getEnvironment' into a @[(Text, Text)]@.
texts :: [(String, String)] -> [(Text, Text)]
texts = fmap $ join bimap pack

{- END AUXILIARY FUNCTIONS -}
