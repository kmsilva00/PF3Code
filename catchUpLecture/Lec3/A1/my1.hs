collatzStep :: Int -> Int
collatzStep n
    | n `mod` 2 == 0 = n`div`2
    | otherwise = 3*n + 1


countCollatzSteps :: Int -> Int
countCollatzSteps 1 = 0
countCollatzSteps n = 1 + countCollatzSteps ( collatzStep n)

stepsToOne :: [(Int,Int)]
