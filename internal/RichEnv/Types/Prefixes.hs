module RichEnv.Types.Prefixes (Prefixes (Prefixes, unPrefixes)) where

import Data.Aeson (FromJSON (parseJSON), Options (unwrapUnaryRecords), ToJSON (toJSON), Value, defaultOptions, genericParseJSON)
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Prefixes = Prefixes {unPrefixes :: HM.HashMap Text [Text]}
  deriving stock (Eq, Show, Generic)

instance FromJSON Prefixes where
  parseJSON :: Value -> Parser Prefixes
  parseJSON = genericParseJSON $ defaultOptions {unwrapUnaryRecords = True}

instance ToJSON Prefixes where
  toJSON :: Prefixes -> Value
  toJSON = toJSON . unPrefixes

instance Semigroup Prefixes where
  (<>) :: Prefixes -> Prefixes -> Prefixes
  (<>) (Prefixes a) (Prefixes b) = Prefixes (a <> b)

instance Monoid Prefixes where
  mempty :: Prefixes
  mempty = Prefixes mempty
