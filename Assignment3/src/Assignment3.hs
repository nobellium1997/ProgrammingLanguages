module A3b where

import Data.Char(isLower)
import Data.List(filter)

-- onlyLowercase
onlyLowercase :: [String] -> [String]
onlyLowercase xs = filter isLowerHelper xs
  where isLowerHelper xs = isLower (head xs)

-- longestString (first version)
longestString :: [String] -> String
longestString [] = ""
longestString xs = foldl (\x y -> if x > y then x else y) "" xs
