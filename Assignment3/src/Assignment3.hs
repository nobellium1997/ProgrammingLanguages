module A3b where

import Data.Char
import Data.List
import Data.Maybe

-- onlyLowercase
onlyLowercase :: [String] -> [String]
onlyLowercase xs = filter isLowerHelper xs
  where
    isLowerHelper xs = isLower (head xs)

-- longestString (first version)
longestString :: [String] -> String
longestString [] = ""
longestString xs =
  foldl
    (\x y ->
       if length x >= length y
         then x
         else y)
    ""
    xs

-- longestString (second version)
longestString' :: [String] -> String
longestString' [] = ""
longestString' xs =
  foldl
    (\x y ->
       if length x > length y
         then x
         else y)
    ""
    xs

-- longestStringHelper
longestStringHelper :: (Int -> Int -> Bool) -> [String] -> String
longestStringHelper _ [] = ""
longestStringHelper f xs = foldl f' "" xs
  where
    f' x y =
      if f (length x) (length y) == True
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
firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b
firstAnswer _ [] = Nothing
firstAnswer f (x:xs) =
  if isNothing (f x)
    then firstAnswer f xs
    else f x

-- allAnswers
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers _ [] = Nothing
allAnswers f (x:xs) = allAnswersHelper f (f x) xs
  where
    allAnswersHelper f acc [] = acc
    allAnswersHelper f acc (x:xs) =
      if isNothing (f x) || isNothing acc
        then Nothing
        else allAnswersHelper f (cat acc (f x)) xs
    cat (Just x) (Just y) = Just (x ++ y)

data Pattern
  = WildcardPat
  | VariablePat (String)
  | UnitPat
  | ConstantPat (Int)
  | ConstructorPat (String, Pattern)
  | TuplePat ([Pattern])
  deriving (Eq, Show)

data Value
  = Constant (Int)
  | Unit
  | Constructor (String, Value)
  | Tuple [Value]
  deriving (Eq, Show)

g f1 f2 pat =
  let r = g f1 f2
   in case pat of
        WildcardPat -> f1 ()
        VariablePat x -> f2 x
        ConstructorPat (_, p) -> r p
        TuplePat values -> foldl (\i p -> (r p) + i) 0 values
        _ -> 0

-- 1
-- Sub problem 1: The arguments g takes is two functions and a pattern.
--                g has to return a number (the type of number depends on the functions being passed)
-- Sub problem 2:
countWildcards :: Pattern -> Int
countWildcards pat = g (\x -> 1) (\y -> 0) pat

-- Sub problem 3:
countWildAndVariableLengths :: Pattern -> Int
countWildAndVariableLengths pat = g (\x -> 1) (\y -> (length y)) pat

-- Sub problem 4:
countAVar :: (String, Pattern) -> Int
countAVar (str, pat) =
  g (\x -> 0)
    (\y ->
       if y == str
         then 1
         else 0)
    pat

-- checkPat
checkPat :: Pattern -> Bool
checkPat pat = checkList (makeList pat)
  where
    makeList (VariablePat x) = [x]
    makeList (ConstantPat x) = [show x]
    makeList (ConstructorPat (_, p)) = makeList p
    makeList (TuplePat values) = foldl (\x y -> (makeList y) ++ x) [] values
    checkList [] = True
    checkList (x:xs) =
      if elem x xs
        then False
        else checkList xs

-- match
match :: (Value, Pattern) -> Maybe [(String, Value)]
match (val, pat) =
  case (val, pat) of
    (_, WildcardPat) -> Just []
    (_, VariablePat s) -> Just [(s, val)]
    (Unit, UnitPat) -> Just []
    (Constant x, ConstantPat y) ->
      if x == y
        then Just []
        else Nothing
    (Constructor (s1, v), ConstructorPat (s2, p)) ->
      if s1 == s2 && (isNothing (match (v, p))) == False
        then Just []
        else Nothing
    (Tuple xs, TuplePat ys) ->
      if length xs == length ys
        then allAnswers match (zip xs ys)
        else Nothing
    (_, _) -> Nothing

-- firstMatch
firstMatch :: Value -> [Pattern] -> Maybe [(String, Value)]
firstMatch val xs = firstAnswer (\x -> match (val, x)) xs
