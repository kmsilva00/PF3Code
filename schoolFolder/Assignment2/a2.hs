generatePrimesUpToN :: Int -> [Int]
generatePrimesUpToN n = sieve [2..n]
  where
    sieve (p:xs)
      | p * p > n = p : xs
      | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]

      
func :: [Int] -> Int -> [[Int]]
func [] _ = [[]]
func [_] 0 = [[]]
func (x:xs) n = verifySum"fc that adds tuple [[0:0]etc]"           ++ func xs n
             

verifySum :: [Int] -> [Int] -> Int -> Bool
verifySum A B n 
  | [ x*y | xs <- A, y <- y:ys ]

[ x*y | xs <- [[1,2,3], [2,3,4]], ys <- [[4,5,6],[5,6,7]], x <- xs, y <- ys ]
-- sum $ zipWith (*) [2,3] [1,1]  

-- totalSteps :: Int -> [Int] -> Int
-- totalSteps n (x:xs) = 



-- testB1 :: Bool
-- testB1 = totalSteps 10 [3, 4] == 2
-- testB2 :: Bool
-- testB2 = totalSteps 10 [11] == 0
-- testB3 :: Bool
-- testB3 = totalSteps 500 [30, 51, 71] == 31
-- testB4 :: Bool
-- testB4 = totalSteps 500 [30, 51, 72] == 0
-- testB5 :: Bool
-- testB5 = totalSteps 1000 [2, 3] == 168