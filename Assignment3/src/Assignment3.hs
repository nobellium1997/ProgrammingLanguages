module A3b where

import Data.Char(isLower)
import Data.List(filter)

onlyLowercase :: [String] -> [String]
onlyLowercase xs = filter isLowerHelper xs
  where isLowerHelper xs = isLower (head xs)

