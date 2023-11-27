-- if we list all the natural numbers below 10 tahat are multiples of 3 or 5, we get 3,5,6 and 9. The sum of these multiples is 23. 
    -- find the sum of all the multiples of 3 or 5 below 1000

sm35 :: Int -> [Int]
sm35 x = [x | x  <- [1..999],(x `mod` 3 == 0) || (x `mod` 5 == 0)]
