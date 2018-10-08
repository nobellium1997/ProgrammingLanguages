module A3b where

import Data.Char
import Data.List

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
longestStringHelper f xs = foldl f' "" xs
  where
  f' x y = if f (length x) (length y) == True
             then x
             else y


-- longestString3
longestString3 :: [String] -> String
longestString3 xs = longestStringHelper (\x y -> x >= y) xs

-- longestString4
longestString4 :: [String] -> String
longestString4 xs = longestStringHelper (\x y -> x > y) xs

-- longestLowercase
longestLowercase :: [String] -> String
longestLowercase xs = (longestString . onlyLowercase) xs

-- revStringRev
revStringRev :: String -> String
revStringRev x = (lowerString . reverse) x
  where
  lowerString [x] = [toLower x]
  lowerString (x:xs) = toLower x : lowerString xs

