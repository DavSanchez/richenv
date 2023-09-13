{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.RichEnv (RichEnv (..), RichEnvItem (..)) where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Aeson (Encoding, FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value, withArray)
import Data.Aeson.Types (Parser)
import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import RichEnv.Types.VarMap (VarMap)
import RichEnv.Types.VarPrefix (VarPrefix)
import RichEnv.Types.VarValue (VarValue)

newtype RichEnv = RichEnv {richEnv :: S.HashSet RichEnvItem}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance Semigroup RichEnv where
  (<>) :: RichEnv -> RichEnv -> RichEnv
  (<>) (RichEnv a) (RichEnv b) = RichEnv $ a <> b

instance Monoid RichEnv where
  mempty :: RichEnv
  mempty = RichEnv mempty

instance FromJSON RichEnv where
  parseJSON :: Value -> Parser RichEnv
  parseJSON = withArray "RichEnv" $ traverse parseJSON >=> (pure . RichEnv . S.fromList . V.toList)

instance ToJSON RichEnv where
  toJSON :: RichEnv -> Value
  toJSON (RichEnv env) = toJSON env

  toEncoding :: RichEnv -> Encoding
  toEncoding (RichEnv env) = toEncoding env

data RichEnvItem
  = -- | Maps an environment variable name to a different one.
    EnvVarNameMap VarMap
  | -- | Sets an environment variable to a specific value.
    EnvVarValue VarValue
  | -- | Maps all environment variables with a certain prefix to a new set of environment variables with a different prefix.
    EnvVarPrefix VarPrefix
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance FromJSON RichEnvItem where
  parseJSON :: Value -> Parser RichEnvItem
  parseJSON v =
    (EnvVarNameMap <$> parseJSON v)
      <|> (EnvVarValue <$> parseJSON v)
      <|> (EnvVarPrefix <$> parseJSON v)

instance ToJSON RichEnvItem where
  toJSON :: RichEnvItem -> Value
  toJSON (EnvVarValue vv) = toJSON vv
  toJSON (EnvVarNameMap vm) = toJSON vm
  toJSON (EnvVarPrefix vp) = toJSON vp

  toEncoding :: RichEnvItem -> Encoding
  toEncoding (EnvVarValue vv) = toEncoding vv
  toEncoding (EnvVarNameMap vm) = toEncoding vm
  toEncoding (EnvVarPrefix vp) = toEncoding vp
