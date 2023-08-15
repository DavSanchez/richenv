module RichEnv (clearEnvironment, richEnv, varMaps, varPrefixes, varValues, RichEnv, RichEnvItem, VarMap, VarPrefix, VarValue) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text, pack, stripPrefix, unpack)
import System.Environment (getEnvironment, setEnv, unsetEnv)

type RichEnv = Set RichEnvItem
type Environment = Map Text Text

data RichEnvItem
    = -- | Maps an environment variable name to a different one.
      EnvVarNameMap VarMap
    | -- | Sets an environment variable to a specific value.
      EnvVarValue VarValue
    | -- | Maps all environment variables with a certain prefix to a new set of environment variables with a different prefix.
      EnvVarPrefix VarPrefix

-- | A mapping from one environment variable name to another.
data VarMap = VarMap
    { mapVarName :: Text
    -- ^ The name of the output environment variable.
    , mapVarFrom :: Text
    -- ^ The name of the input environment variable.
    }
    deriving stock (Eq, Ord)

data VarValue = VarValue
    { valueName :: Text
    -- ^ The name of the environment variable.
    , valueValue :: Text
    -- ^ The value of the environment variable.
    }
    deriving stock (Eq, Ord)

-- | A prefix to add to all environment variables.
data VarPrefix = VarPrefix
    { prefixName :: Text
    -- ^ The prefix of the output environment. Can be empty or the same as @prefixFrom@.
    , prefixFrom :: Text
    -- ^ The prefix of the input environment.
    }
    deriving stock (Eq, Ord)

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

{- FILTERING TYPE VARIANTS -}

-- | Gets only the 'VarMap' items from a 'RichEnv'.
varMaps :: RichEnv -> Set VarMap
varMaps = S.foldr f S.empty
  where
    f (EnvVarNameMap vm) = S.insert vm
    f _ = id

-- | Gets only the 'VarValue' items from a 'RichEnv'.
varValues :: RichEnv -> Set VarValue
varValues = S.foldr f S.empty
  where
    f (EnvVarValue vv) = S.insert vv
    f _ = id

-- | Gets only the 'VarPrefix' items from a 'RichEnv'.
varPrefixes :: RichEnv -> Set VarPrefix
varPrefixes = S.foldr f S.empty
  where
    f (EnvVarPrefix vp) = S.insert vp
    f _ = id

{- END FILTERING TYPE VARIANTS -}

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
