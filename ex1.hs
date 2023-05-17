funcaoum :: Double -> Double
funcaoum x
    | x >= 0 = (x+4)/(x+2)
    | otherwise = 2/x


funcaodois :: Double -> Double -> Double 
funcaodois x y
    | x >= y = x + y 
    | otherwise = x - y 

funcaotres :: Double -> Double -> Double -> Double
funcaotres x y z    
    | x + y > z = x + y + z
    | x + y < z = x - y - z
    | otherwise = 0
 
