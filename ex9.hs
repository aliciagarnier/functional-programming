howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples x y z
    | z < y  = 0 
    | z `mod` x == 0 = 1 + howManyMultiples x y (z-1)
    | otherwise = howManyMultiples x y (z-1)
