{-# LANGUAGE InstanceSigs #-}

module RichEnv.Types.ParseError (RichEnvParseError (..)) where

import Control.Exception (Exception (displayException))

data RichEnvParseError
  = VarMapWildcards
  | VarMapMissingFields
  | VarPrefixInvalidWildcards
  | VarPrefixMissingFields
  | VarValueNoName
  deriving stock (Eq, Show)

instance Exception RichEnvParseError where
  displayException :: RichEnvParseError -> String
  displayException VarMapWildcards = "VarMap `name` or `from` cannot contain wildcard character `*`"
  displayException VarMapMissingFields = "VarMap must have fields `name` and `from`"
  displayException VarPrefixInvalidWildcards = "VarPrefix `name` and `from` must end with a `*` and not contain `*` anywhere else."
  displayException VarPrefixMissingFields = "VarPrefix must have fields `name` and `from`"
  displayException VarValueNoName = "VarValue must have field `name`"
