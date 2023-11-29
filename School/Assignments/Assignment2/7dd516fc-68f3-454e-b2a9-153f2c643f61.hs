import Data.List ( maximumBy, nub, sort )

palindrome :: (Ord a) => [a] -> Bool
palindrome xs = reverse xs == xs   


subsequence :: [a] -> [[a]]
subsequence [] = [[]]
subsequence (a:as) = map (a:) (subsequence as) ++ (subsequence as)


compareLength :: [a] -> [a] -> Ordering
compareLength comparatea comparateb = compare (length comparatea) (length comparateb) 


longestValidSubsequence :: ([a] -> Bool) -> [a] -> [a]
longestValidSubsequence condition str
  | null validSubsequences = []
  | otherwise = maximumBy compareLength validSubsequences
  where
    validSubsequences = filter condition (subsequence str)

-- -- -- -- -- -- -- -- -- -- -- -- 

-- ex 2 --

isPrime :: Int -> Bool
isPrime k
  | k <= 1 = False
  | otherwise = all (\n -> k `mod` n /= 0) [2..isqrt k]
  where isqrt = floor . sqrt . fromIntegral

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
  
generateCombinations :: Int -> [Int] -> [[Int]]
generateCombinations n stepSizes =
  nub [combination | combination <- sequence [ [0, step .. n] | step <- stepSizes], sum combination <= n]

primeSumCombinations :: Int -> [Int] -> [[Int]]
primeSumCombinations n stepSizes =
  filter (\combination -> isPrime (sum combination)) (nub $ generateCombinations n stepSizes)

summapPSC :: Int -> [Int] -> [[Int]]
summapPSC n stepSizes =
  map (\combination -> [sum combination]) (primeSumCombinations n stepSizes)

totalSteps :: Int -> [Int] -> Int
totalSteps upperBound steps = length $ removeDuplicates $ summapPSC upperBound steps  