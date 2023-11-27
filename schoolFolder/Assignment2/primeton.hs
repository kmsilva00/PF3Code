generatePrimesUpToN :: Int -> [Int]
generatePrimesUpToN n = sieve [2..n]
  where
    sieve (p:xs)
      | p * p > n = p : xs
      | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]

      
