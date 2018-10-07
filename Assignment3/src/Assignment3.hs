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
longestString xs = foldl (\x y -> if length x >= length y then x else y) "" xs

-- longestString (second version)
longestString' :: [String] -> String
longestString' xs = foldl (\x y -> if length x > length y then x else y) "" xs
