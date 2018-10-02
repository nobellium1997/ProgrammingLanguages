module A2b where

-- Remove All except
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept a (x:xs) =
  if (a /= x)
    then removeAllExcept a xs
    else x : removeAllExcept a xs

-- Remove All
removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll a (x:xs) =
  if (a == x)
    then removeAll a xs
    else x : removeAll a xs

-- substitute
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute a b (x:xs) =
  if (a == x)
    then b : substitute a b xs
    else x : substitute a b xs

-- mergeSorted3
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = []
mergeSorted3 xs ys zs = mergeHelper zs (mergeHelper xs ys)
  where
    mergeHelper xs [] = xs
    mergeHelper [] ys = ys
    mergeHelper (x:xs) (y:ys) =
      if (x < y)
        then x : mergeHelper xs (y : ys)
        else y : mergeHelper (x : xs) ys

-- Defined TriTree data structure
data TriTree a
  = EmptyNode
  | TriNode a
            (TriTree a)
            (TriTree a)
            (TriTree a)
  deriving (Show)

instance (Eq a) => Eq (TriTree a) where
  EmptyNode == EmptyNode = True
  TriNode a la ma ra == TriNode b lb mb rb =
    (a == b) && (la == lb) && (ma == mb) && (ra == rb)
  _ == _ = False

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

-- inTree
inTree :: Eq a => a -> TriTree a -> Bool
inTree _ EmptyNode = False
inTree x (TriNode value left middle right)
  | x == value = True
  | otherwise = inTree x left || inTree x middle || inTree x right

-- leaf list
leafList :: TriTree a -> [a]
leafList EmptyNode = []
leafList (TriNode value EmptyNode EmptyNode EmptyNode) = [value]
leafList (TriNode value left middle right) =
  leafList left ++ leafList middle ++ leafList right

-- inOrderMap
inOrderMap :: (a -> b) -> TriTree a -> TriTree b
inOrderMap _ EmptyNode = EmptyNode
inOrderMap f (TriNode value left middle right) =
  (TriNode
     (f value)
     (inOrderMap f left)
     (inOrderMap f middle)
     (inOrderMap f right))

-- preOrderFold
preOrderFold :: (b -> a -> b) -> b -> TriTree a -> b
preOrderFold _ x EmptyNode = x
preOrderFold f x (TriNode value left middle right) =
  preOrderFold f (preOrderFold f (preOrderFold f (f x value) left) middle) right
