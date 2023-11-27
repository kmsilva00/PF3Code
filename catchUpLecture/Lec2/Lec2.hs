max' x y = if x > y then x else y 

iseven' x = if x `mod` 2 == 0 then True else False

ageGroup' x = if x < 18 then "Teen" else if x < 68 then "Adult" else "Old"

ageGroup'' x
    | x < 18 = "Teen"
    | x < 65 = "Adult"
    | otherwise = "old"

abs' :: Int -> Int
abs' n 
    | n >= 0 = n
    | otherwise = -1 * n

charToDigit :: Char -> Int
charToDigit c =
    case c of 
        '0' -> 0
        '1' -> 1
        _ -> -1

ones = 1 : ones

powersOfTwo = iterate (*2) 1

weekDays = cycle ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]

primes = filterPrime [2..]
    where filterPrime (p:xs) =
                p : filterPrime [x | x <- xs, x `mod` p /= 0]

fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)



fibs :: [Int]
fibs = gen 1 2
    where
        gen :: Int -> Int -> [Int]
        gen x y = x : gen y (x+y)

factors' :: Integral a => a -> [a]
factors' n = filter (\x -> (n `mod` x) == 0) [1..n]
isPrime' :: Integral a => a -> Bool
isPrime' n = factors' n == [1,n]
primeFactors' :: Integral a => a -> [a] 
primeFactors' n = filter isPrime' candidates
 where candidates = filter (\x -> (n `mod` x) == 0) [2..(ceiling $ sqrt $ fromIntegral n)]
-- primeFactors 13195
--primeFactors 600851475143


factors :: Integral a => a -> [a]
factors x = [n | n <- [1..x], x `mod` n == 0]

isPrime :: Integral a => a -> Bool
isPrime x = factors x == [1,x] 

primeFactors :: Integral a => a -> [a]
primeFactors n = filter isPrime [ x | x <- [2.. ceiling $ sqrt $ fromIntegral n], (n `mod` x) == 0]


bmi ::  Float -> Float -> String
bmi weight height
    | bmi <= 18.5 = "underweight"
    | bmi <= 25 = "Normal"
    | otherwise = "Obese"
    where
        bmi = weight / height^2 

