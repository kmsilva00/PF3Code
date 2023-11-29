--https://www.codewars.com/kata/5acc79efc6fde7838a0000a0/train/haskell

occurs :: Int -> Tree Int -> Bool
occurs n Nil = False
occurs n (Node l m r) | m == n = True
                      | otherwise = occurs n l || occurs n r


--------------------
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show)

search :: Int -> Tree Int -> Bool
search _ Nil = False
search n (Node l v r)
    | n == v = True
    | otherwise = search n l || search n r

    