module Assignment2
    ( removeAllExcept
    ) where

removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept a [] = []
removeAllExcept a (x:xs) = if (a == x) then removeAllExcept a xs else x : removeAllExcept a xs
