module Task6
  ( foo
  ) where

import Data.Maybe (mapMaybe)

-- distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True -> Just $ exp pi
      False -> Nothing
