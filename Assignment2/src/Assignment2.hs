module Assignment2
    ( removeAllExcept,
      removeAll
    ) where

-- Remove All except
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept a [] = []
removeAllExcept a (x:xs) = if (a /= x) then removeAllExcept a xs else x : removeAllExcept a xs

-- Remove All
removeAll :: Eq a => a -> [a] -> [a]
removeAll a [] = []
removeAll a (x:xs) = if (a == x) then removeAll a xs else x : removeAll a xs

