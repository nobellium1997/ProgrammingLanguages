module Assignment2
    ( removeAllExcept,
      removeAll,
      substitute,
      mergeSorted3
    ) where

-- Remove All except
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept a [] = []
removeAllExcept a (x:xs) = if (a /= x) then removeAllExcept a xs else x : removeAllExcept a xs

-- Remove All
removeAll :: Eq a => a -> [a] -> [a]
removeAll a [] = []
removeAll a (x:xs) = if (a == x) then removeAll a xs else x : removeAll a xs

-- substitute
substitute :: Eq a => a -> a -> [a] -> [a]
substitute a b [] = []
substitute a b (x:xs) = if (a == x) then b : substitute a b xs else x : substitute a b xs

-- mergeSorted3
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]

