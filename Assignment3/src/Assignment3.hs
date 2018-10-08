module A3b where

import Data.Char
import Data.List
import Data.Maybe

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

-- firstAnswer
firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
firstAnswer _ [] = Nothing
firstAnswer f (x:xs) = if isNothing (f x) then firstAnswer f xs else f x

-- allAnswers
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers _ [] = Nothing
allAnswers f (x:xs) = allAnswersHelper f (f x) xs
  where
  allAnswersHelper _ acc [] = acc
  allAnswersHelper f acc (x:xs) = if isNothing (f x)
                                    then Nothing
                                    else allAnswersHelper f ((f x)) xs
