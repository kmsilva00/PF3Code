nbYears :: Int -> Float -> Int -> Int -> Int
nbYears p0 percent aug p
    | p0 >= p = 0
    | otherwise = 1 + nbYears (floor(fromIntegral p0 * (1 + percent/100)) + aug ) percent aug p