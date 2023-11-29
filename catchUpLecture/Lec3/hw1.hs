collatzStep :: Int -> Int
collatzStep n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3*n+1

countCollatzSteps :: Int -> Int
countCollatzSteps 1 = 0
countCollatzSteps n = 1 + countCollatzSteps ( collatzStep n)

stepsToOne :: [(Int,Int)]
stepsToOne = [(n, countCollatzSteps n) | n <- [1..] ]

type Time = (Int, Int)
--tomin fn that converts the Time tuple into overall minutes
tomin :: Time -> Int
tomin x =  60 * fst x + snd x

goToBed :: [Time] -> Int -> Time
goToBed (x:xs) n
    | xs == [] = x 
    | tomin(xs!!0) - tomin(x) >= n = x 
    | otherwise = goToBed xs n
