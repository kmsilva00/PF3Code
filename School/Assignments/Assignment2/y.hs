myfn :: Int -> [Int] -> Int -> [Int] -> Int -> [[Int]]
myfn _ _ _ _ 0 = [[]]
myfn _ _ _ [] _ = []
myfn n x5 pos (y:ys) n1 = if pos == length x5
    then [[y]]
    else
      map (y:) (myfn n x5 (pos+1) [0..n `div` (x5!!(pos+1))] (n1-1)) ++ (myfn n x5 pos ys n1)

totalSteps :: Int -> [Int] -> Int
totalSteps n x5 = length (myfn n x5 0 [0..3] 2)

testB1 :: Bool
testB1 = totalSteps 10 [3, 4] == 2

testB2 :: Bool
testB2 = totalSteps 10 [11] == 0

testB3 :: Bool
testB3 = totalSteps 500 [30, 51, 71] == 31

testB4 :: Bool
testB4 = totalSteps 500 [30, 51, 72] == 0

testB5 :: Bool
testB5 = totalSteps 1000 [2, 3] == 168