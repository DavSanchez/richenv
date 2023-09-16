module RichEnv.Types.Values (Values (Values, unValues)) where

import Data.Aeson (FromJSON (parseJSON), Options (unwrapUnaryRecords), ToJSON (toJSON), Value, defaultOptions, genericParseJSON)
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Values = Values {unValues :: HM.HashMap Text Text}
  deriving stock (Eq, Show, Generic)

instance FromJSON Values where
  parseJSON :: Value -> Parser Values
  parseJSON = genericParseJSON $ defaultOptions {unwrapUnaryRecords = True}

instance ToJSON Values where
  toJSON :: Values -> Value
  toJSON = toJSON . unValues

instance Semigroup Values where
  (<>) :: Values -> Values -> Values
  (<>) (Values a) (Values b) = Values (a <> b)

instance Monoid Values where
  mempty :: Values
  mempty = Values mempty
