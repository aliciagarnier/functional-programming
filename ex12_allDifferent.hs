allDifferent :: Int->Int->Int->Bool
allDifferent m n p = (m/= n) && (n /= p)

{-
a) O que está errado nesa definição -}

allDifferentRight :: Int->Int->Int->Bool
allDifferentRight m n p = (m/= n) && (n /= p) && (m/= p)