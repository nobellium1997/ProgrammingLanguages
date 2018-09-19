module A1c where

-- Dot Product
sDotProduct s (x1, y1) (x2, y2) = ((x1 * x2) + (y1*y2)) * s

-- Distance
distance (x1, y1) (x2, y2)  = sqrt ((x2 - x1) ^ 2  + (y2 - y1) ^ 2)

-- Triple Distance
tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt ((x2-x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)

-- Find Min
findMin [] = 0
findMin [x] = x
findMin (x:y:xs) = if (x < y) then findMin(x:xs) else findMin(y:xs)

-- Tuple Dot Product
tupleDotProduct [] [] = 0
tupleDotProduct (q:qs) (p:ps) = (q * p) + tupleDotProduct qs ps

-- revZip2Lists
revZip2Lists [] [] = []
revZip2Lists [x] [y] = [(y, x)]
revZip2Lists (x:xs) (y:ys) = revZip2Lists (xs) (ys) ++ [(y, x)]

-- everyThird
everyThird [] = []
everyThird [x] = []
everyThird [x,y] = []
everyThird (x:y:z:xs) = [z] ++ everyThird (xs)

-- minMax
minMax (x:y:xs) = minMaxHelper x y xs
  where
    minMaxHelper min max [] = (min,max)
    minMaxHelper min max [x] = if (x < min)
                                  then (x, max)
                                  else if (x > max)
                                      then (min, x)
                               else (min, max)
    minMaxHelper min max (x:y:xs) = if (x < y)
                                       then if (x < min && y > max)
                                            then minMaxHelper x y xs
                                                else if (x < min)
                                                     then minMaxHelper x max xs
                                                else if (y > max)
                                                     then minMaxHelper min y xs
                                            else minMaxHelper min max xs
                                     else if (y < x)
                                          then if (y < min && x > max)
                                               then minMaxHelper y x xs
                                               else if (y < min)
                                                    then minMaxHelper y max xs
                                               else if (x > max)
                                                    then minMaxHelper min x xs
                                          else minMaxHelper min max xs
                                     else minMaxHelper min max xs

