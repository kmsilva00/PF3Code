product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product xs

length' :: [Int] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd(n-1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven(n-1)


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smallerThanX ++ [x] ++ biggerThanX
    where
        smallerThanX = [ v | v <- xs, v < x]
        biggerThanX = [ v | v <- xs, v >= x]


drop' :: Integral b => b -> [a] -> [a]
drop' _ []      = []
drop' 0 xs      = xs
drop' n (x:xs)  = drop'(n-1) xs

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x: init' xs
    

-- https://www.codewars.com/kata/563b662a59afc2b5120000c6/train/haskell
nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug surpass 
    | p0 >= surpass = 0
    | otherwise = 1 + nbYear (( p0 + round(fromIntegral p0 * percent/100) )+aug ) percent aug surpass 
