-- Problem 1 : Find the last element of a list.
 
-- you can call last, otherwise
last' :: [a] -> a 
last' x = head (reverse x)

-- Problem 2 :  Find the last but one element of a list.

butLast :: [a] -> a 
butLast [b , _ ]= b
butLast (_:xs) = butLast xs 

butLast' :: [a] -> a 
butLast' x =  reverse x !! 1 

butLast'' :: [a] -> a 
butLast'' x = (!!) (reverse x) 1 


list :: [Integer]
list  = [1..100]

main :: IO ()
main = do
    print $ butLast list
