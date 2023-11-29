max' :: Ord a => a -> (a -> a) 
max' = \x -> ( \y -> if x>=y then x else y)

max'' :: Ord a => (a,a) -> a
max'' (x,y) = if x>=y then x else y

isUpperCase :: Char -> Bool
isUpperCase x = (`elem` ['A'..'Z']) x

-- map isUpperCase "Hello World!"
-- filter odd [1..100]
-- filter isUpperCase "HELLO world"
-- takeWhile (>5) [6,7,10,2,20]
-- [6,7,10]
-- dropWhile (>5) [6,7,10,2,20]
-- [2,20]
-- all (>5) [1,2,3,9,4,6]
-- False
-- any (>5) [1,2,3,9,4,6]
-- any (\x -> x `mod` 2 == 0) [3,5,7,11]
-- any (even) [3,5,7,11]
-- map (^2) [1,2,3,4]

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

sum'' :: [Int] -> Int
sum'' xs = foldr (+) 0 xs

product'' :: [Int] -> Int
product'' xs = foldr (*) 1 xs

or' :: [Bool] -> Bool
or' xs = foldr (||) False xs

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

reverse' xs = foldl (flip (:)) [] xs

reverse'' xs = foldl (\zs x -> x :zs) [] xs

twice f x = f (f x)

sumsqeven = sum . map (^2) . filter (even)


length'' :: [a] -> Int
length'' = foldr (\_ n -> 1 + n ) 0

reverse''' xs = foldl (\xz x -> x :xz) [] xs


-- concat [[1, 2], [3, 4], [], [5]] == [1, 2, 3, 4, 5]
-- Write a function foldConcat , equivalent to concat , with two foldr :
