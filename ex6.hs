iesimo :: Int -> Float
iesimo 0 = sqrt 6
iesimo x = sqrt(6 + iesimo (x - 1))