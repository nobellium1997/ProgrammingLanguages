module Assignment2
    ( removeAllExcept,
      removeAll,
      substitute,
      mergeSorted3,
      nodeValue,
      leftChild,
      middleChild
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
mergeSorted3 xs ys zs = mergeHelper zs (mergeHelper xs ys)
  where
    mergeHelper xs [] = xs
    mergeHelper [] ys = ys
    mergeHelper (x:xs) (y:ys) = if(x < y) then x : mergeHelper xs (y:ys) else y : mergeHelper (x:xs) ys

-- Defined TriTree data structure
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)

instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

-- node Value
nodeValue :: TriTree a -> a
nodeValue (TriNode value left middle right) = value
nodeValue EmptyNode = error "Passed in empty tree"

-- left child
leftChild :: TriTree a -> TriTree a
leftChild (TriNode value left middle right) = left
leftChild EmptyNode = error "Passed in empty tree"

-- middle child
middleChild :: TriTree a -> TriTree a
middleChild (TriNode value left middle right) = middle
middleChild EmptyNode = error "Passed in empty tree"

-- right child
rightChild :: TriTree a -> TriTree a
rightChild (TriNode value left middle right) = right
rightChild EmptyNode = error "Passed in empty tree"
