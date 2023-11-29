import Data.List (nub)

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
  

-- Example usage:
main :: IO ()
main = do
  let n = 1000
  let stepSizes = [2,3]
  let primeCombinations = length $ primeSumCombinations n stepSizes
  print primeCombinations
  
  print primeCombinations
