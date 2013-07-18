module Prob10 where
-- <>

isPrime :: Int -> Bool
isPrime n 
        | n < 2 = False
        | n == 2 = True
        | mod n 2 == 0 = False
        | otherwise = not $ any divisible numbers 
        where 
                divisible = \x -> mod n x == 0
                numbers = takeWhile (\x -> x <= p) [3, 5 .. p]
                p = floor $ sqrt $ fromIntegral n
                
prob = sum $ filter isPrime [2..2000000]