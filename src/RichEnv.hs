module RichEnv (clearEnvironment, toEnvList, setRichEnv) where

import Data.Text qualified as T
import RichEnv.Setters (mappingsToValues, prefixesToValues, richEnvToValues, valuesToEnv, valuesToEnvList)
import RichEnv.Types (Environment, toEnvironment)
import RichEnv.Types.RichEnv (RichEnv (..))
import System.Environment (getEnvironment, unsetEnv)

-- | Returns a list of environment variables abiding to the 'RichEnv' rules.
toEnvList :: RichEnv -> IO Environment
toEnvList re = valuesToEnvList . newEnvSet . toEnvironment <$> getEnvironment
  where
    vvs = values re
    vms = flip mappingsToValues (mappings re)
    vps = flip prefixesToValues (prefixes re)
    newEnvSet cEnv = vvs <> vms cEnv <> vps cEnv

-- | Sets the environment variables available to the current process abiding to the 'RichEnv' rules.
setRichEnv :: RichEnv -> IO ()
setRichEnv re = do
  currentEnv <- toEnvironment <$> getEnvironment
  clearEnvironment currentEnv
  valuesToEnv (richEnvToValues currentEnv re)

-- | Clears all environment variables of the current process.
--
-- >>> import System.Environment
-- >>> env <- getEnvironment
-- >>> clearEnvironment env
-- >>> env' <- getEnvironment
-- >>> null env'
-- True
-- >>> import System.Environment
-- >>> env <- getEnvironment
-- >>> clearEnvironment env
-- >>> setEnv "FOO" "bar"
-- >>> env' <- getEnvironment
-- >>> env' == [("FOO", "bar")]
-- True
clearEnvironment :: Environment -> IO ()
clearEnvironment = mapM_ (unsetEnv . T.unpack . fst)
