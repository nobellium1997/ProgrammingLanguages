module Assignment1
    ( sDotProduct,
    distance,
    tripleDistance,
    findMin,
    tupleDotProduct
    ) where
-- Dot Product
sDotProduct  x (arg1, arg2) (arg3, arg4) = ((arg1 * arg2) + (arg3 * arg4)) * x

-- Distance
distance (x1, y1) (x2, y2)  = sqrt ((x2 - x1) ^ 2  + (y2 - y1) ^ 2)

-- Triple Distance
tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt ((x2-x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)

-- Find Min
findMin [x] = x
findMin (x:y:xs) = if (x < y) then findMin(x:xs) else findMin(y:xs)

-- Tuple Dot Product
tupleDotProduct [] [] = 0
tupleDotProduct (q:qs) (p:ps) = (q * p) + tupleDotProduct qs ps