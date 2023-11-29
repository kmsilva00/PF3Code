isPrime :: Int -> Bool
isPrime k
  | k <= 1 = False
  | otherwise = all (\n -> k `mod` n /= 0) [2..isqrt k]
  where isqrt = floor . sqrt . fromIntegral
