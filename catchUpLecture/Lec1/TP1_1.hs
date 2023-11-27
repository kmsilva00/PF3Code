import Data.ByteString (find)
-- Problem 1 : Find the last element of a list.
 
-- you can call last, otherwise
last' :: [a] -> a 
last' xs = head $ reverse xs
-- Problem 2 :  Find the last but one element of a list.

butLast :: [a] -> a 
butLast [b, _] = b
butLast (x:xs) = butLast xs

butLast' :: [a] -> a 
butLast' a = a !! ((length a) -2)

butLast'' :: [a] -> a 
butLast'' a = reverse a !! 1

list :: [Int]
list = [1..]

-- Find the k th element of a list, but the first element in the list is number 1. (TODO)
findK :: [Integer] -> Integer -> Maybe Integer
findK [] _ = Nothing
findK (x:xs) n 
    | x == n = Just x
    | otherwise = findK xs n

findProduct :: Int -> Int
findProduct x = product (take x (list))


squarelist :: [Int]
squarelist = [x^2 | x <- list]


squareListS :: Int -> [Int]
squareListS s = [x | x <- squarelist, x<=s] 