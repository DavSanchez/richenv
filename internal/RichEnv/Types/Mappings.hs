module RichEnv.Types.Mappings (Mappings (Mappings, unMappings)) where

import Data.Aeson (FromJSON (parseJSON), Options (unwrapUnaryRecords), ToJSON (toJSON), Value, defaultOptions, genericParseJSON)
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Mappings = Mappings {unMappings :: HM.HashMap Text Text}
  deriving stock (Eq, Show, Generic)

instance FromJSON Mappings where
  parseJSON :: Value -> Parser Mappings
  parseJSON = genericParseJSON $ defaultOptions {unwrapUnaryRecords = True}

instance ToJSON Mappings where
  toJSON :: Mappings -> Value
  toJSON = toJSON . unMappings

instance Semigroup Mappings where
  (<>) :: Mappings -> Mappings -> Mappings
  (<>) (Mappings a) (Mappings b) = Mappings (a <> b)

instance Monoid Mappings where
  mempty :: Mappings
  mempty = Mappings mempty
