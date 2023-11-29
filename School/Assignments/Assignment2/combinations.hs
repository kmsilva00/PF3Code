-- asc :: Int -> Int -> [Int]
-- asc n m = n : m : asc (n+m) n

-- Function to generate a list of prime numbers up to n
generatePrimesUpToN :: Int -> [Int]
generatePrimesUpToN n = sieve [2..n]
  where
    sieve (p:xs)
      | p * p > n = p : xs
      | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Function to remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Function to find prime-numbered steps you can reach
totalSteps :: Int -> [Int] -> Int
totalSteps n allowedStepSizes = length $ removeDuplicates $ filter (`elem` primeNumbersUpToN) reachableSteps
  where
    primeNumbersUpToN = generatePrimesUpToN n
    reachableSteps = 1 : concatMap (\stepSize -> [x | x <- reachableSteps, x + stepSize <= n]) allowedStepSizes

-- Test cases
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
