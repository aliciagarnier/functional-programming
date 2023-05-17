
reverso :: [Int] -> [Int]
reverso [] = [] 
reverso (a:b) = reverso b ++ [a]

