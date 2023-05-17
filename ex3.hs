soma :: Int -> Int -> Int
soma x y = x + y 

mult :: Int -> Int -> Int
mult 0 _  = 0 
mult x y = mult (x-1) y + soma 0 y

mult2 :: Int -> Int -> Int
mult2 0 _ = 0 
mult2 x y = soma x (mult2 x (y-1))

