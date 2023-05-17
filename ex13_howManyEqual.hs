howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | (x==y) && (y==z) = 3
    | x == y = 2
    | otherwise = 0