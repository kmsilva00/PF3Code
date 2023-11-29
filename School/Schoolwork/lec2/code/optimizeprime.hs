factors :: Integral a => a -> [a]
factors n = filter (\x -> (n `mod` x) == 0) [1..n]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1,n]

primeFactors :: Integral a => a -> [a]
primeFactors n = filter isPrime candidates
    where
        m = round $ fromIntegral n
        candidates = filter (\x -> (n `mod` x) == 0) [2..m]

--primeFactors 13195


--FIX