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
longestString' [] = ""
longestString' xs = foldl (\x y -> if length x > length y then x else y) "" xs

-- longestStringHelper
longestStringHelper :: (Int -> Int -> Bool) -> [String] -> String
longestStringHelper _ [] = ""
longestStringHelper f xs = longestStringHelper' f (head xs) xs
  where
  longestStringHelper' f acc [x] = if f (length acc) (length x)
                                      then acc
                                      else x
  longestStringHelper' f acc (x:xs) = if (f (length acc) (length x))
                                        then longestStringHelper' f acc xs
                                        else longestStringHelper' f x xs

-- longestString3
longestString3 :: [String] -> String
longestString3 xs = longestStringHelper (\x y -> x >= y) xs

-- longestString4
longestString4 :: [String] -> String
longestString4 xs = longestStringHelper (\x y -> x > y) xs


