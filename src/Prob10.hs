module Prob10 where
-- <>

isPrime :: Int -> Bool
isPrime n 
        | n < 2 = False
        | n == 2 = True
        | mod n 2 == 0 = False
        | otherwise = not $ any divisible [3, 5 .. i]
        where 
                divisible x = mod n x == 0              
                i = floor $ sqrt (fromIntegral n :: Double)
      
main :: Int                
main = sum $ filter isPrime [2..2000000]