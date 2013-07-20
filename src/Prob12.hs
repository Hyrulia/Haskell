module Prob12 where
-- <>

prob :: Int
prob = head $ filter ((>=500) . divisors) triangle
        where   triangle = scanl1 (+) [1..]
                divisors n = sum [2 | a <- [1..(truncate $ sqrt (fromIntegral n :: Double))], mod n a == 0]
                 