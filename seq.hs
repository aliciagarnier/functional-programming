sequencia :: Int->Int
sequencia 0 = 0 
sequencia x = x + sequencia(x-1)