module RichEnv (clearEnvironment, toEnvList, setRichEnv, RichEnv, VarMap (..), VarPrefix (..), VarValue (..)) where

import RichEnv.Filters (varMaps, varPrefixes, varValues)
import RichEnv.Setters (setPrefixedVars, setVarMapValues, setVarValueEnv, varValuesToEnvironment)
import RichEnv.Types (Environment, RichEnv, VarMap (..), VarPrefix (..), VarValue (..))
import System.Environment (getEnvironment, unsetEnv)

-- | Returns a list of environment variables abiding to the 'RichEnv' rules.
toEnvList :: RichEnv -> IO Environment
toEnvList re = varValuesToEnvironment . newEnvSet <$> getEnvironment
  where
    vvs = varValues re
    vms = flip setVarMapValues (varMaps re)
    vps = flip setPrefixedVars (varPrefixes re)
    newEnvSet cEnv = vvs <> vms cEnv <> vps cEnv

-- | Sets the environment variables available to the current process abiding to the 'RichEnv' rules.
setRichEnv :: RichEnv -> IO ()
setRichEnv re = do
  currentEnv <- getEnvironment
  clearEnvironment currentEnv
  mapM_ setVarValueEnv (newEnvSet currentEnv)
  where
    vvs = varValues re
    vms = flip setVarMapValues (varMaps re)
    vps = flip setPrefixedVars (varPrefixes re)
    newEnvSet cEnv = vvs <> vms cEnv <> vps cEnv

-- | Clears all environment variables of the current process.
--
-- >>> import System.Environment
-- >>> (getEnvironment >>= clearEnvironment) >> getEnvironment >>= pure . null
-- True
-- >>> import System.Environment
-- >>> (getEnvironment >>= clearEnvironment) >> setEnv "FOO" "bar" >> getEnvironment >>= \s -> pure (s == [("FOO", "bar")])
-- True
clearEnvironment :: Environment -> IO ()
clearEnvironment = mapM_ (unsetEnv . fst)
