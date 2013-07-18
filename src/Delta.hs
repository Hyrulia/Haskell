module Delta where

deltaSol :: Float -> Float -> Float -> Float
deltaSol a b c 
        | x > 0 = 2
        | x == 0 = 1
        | otherwise = 0
        where x = b*b - 4 * a * c

delta ::  Float -> Float -> Float -> [Float]
delta a b c 
        | deltaSol a b c == 2 = [ y ,  z]
        | deltaSol a b c == 1 = [-b/a]
        | otherwise = []
        where   x = b*b - 4 * a * c
                y = ( -b - sqrt x ) / 2*a
                z = ( -b + sqrt x ) / 2*a
