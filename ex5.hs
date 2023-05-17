
{- definição de uma função fourpower que retorna seu segundo argumento elevado a quarta potência utilizando a função square (quadrado) -}
square :: Int -> Int
square x = x * x

fourPower :: Int -> Int 
fourPower 0 = 0 
fourPower y = square y * square y