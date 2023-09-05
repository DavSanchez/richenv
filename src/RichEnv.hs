module RichEnv (clearEnvironment, richEnv, RichEnv, VarMap, VarPrefix, VarValue) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Text (Text, pack)
import RichEnv.Filters (varMaps, varPrefixes, varValues)
import RichEnv.Setters (setPrefixedVars, setVarMapValues, setVarValueEnv)
import RichEnv.Types (RichEnv, VarMap (..), VarPrefix (..), VarValue (..))
import System.Environment (getEnvironment, unsetEnv)

-- | Clears all environment variables of the current process.
--
-- >>> import System.Environment
-- >>> (getEnvironment >>= clearEnvironment) >> getEnvironment >>= pure . null
-- True
-- >>> import System.Environment
-- >>> (getEnvironment >>= clearEnvironment) >> setEnv "FOO" "bar" >> getEnvironment >>= \s -> pure (s == [("FOO", "bar")])
-- True
clearEnvironment :: [(String, String)] -> IO ()
clearEnvironment = mapM_ (unsetEnv . fst)

-- | Sets the environment variables available to the current process abiding to the 'RichEnv' rules.
richEnv :: RichEnv -> IO ()
richEnv re = do
  currentEnv <- getEnvironment
  clearEnvironment currentEnv
  mapM_ setVarValueEnv (newEnvSet currentEnv)
  where
    vvs = varValues re
    vms = flip setVarMapValues (varMaps re)
    vps = flip setPrefixedVars (varPrefixes re)
    newEnvSet cEnv = vvs <> vms (texts cEnv) <> vps (texts cEnv)

{- AUXILIARY FUNCTIONS -}

-- | Converts the '[(String, String)]@ returned by 'getEnvironment' into a @[(Text, Text)]@.
texts :: [(String, String)] -> [(Text, Text)]
texts = fmap $ join bimap pack

{- END AUXILIARY FUNCTIONS -}
