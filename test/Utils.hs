module Utils (nonEmptyVarMap, nonEmptyVarValue) where

import Data.List.NonEmpty (fromList)
import RichEnv.Types.VarMap (VarMap (..))
import RichEnv.Types.VarValue (VarValue (..))

nonEmptyVarValue :: String -> String -> VarValue
nonEmptyVarValue v = VarValue (fromList v)

nonEmptyVarMap :: String -> String -> VarMap
nonEmptyVarMap v f = VarMap (fromList v) (fromList f)
