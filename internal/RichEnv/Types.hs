module RichEnv.Types (Environment, NonEmptyString) where

import Data.List.NonEmpty qualified as NE

type Environment = [(String, String)]

type NonEmptyString = NE.NonEmpty Char
