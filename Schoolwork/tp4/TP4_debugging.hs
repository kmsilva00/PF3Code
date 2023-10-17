--Exercise 1: Define concatt :: [[a]] -> [a] that concatenate a list of lists.

concatt :: [[a]] -> [a]
concatt (z:zs) = z ++ concatt zs

test1 :: [Char]
test1 = concatt ["this", " is", " a", " sentence!" ]

--Exercise 2: Define a function that produces a list with n identical elements.
replicatee  :: Int -> a -> [ a ]
replicatee 0 _ = []
replicatee n x = x : replicatee n x 

test2 :: [[Char]]
test2 = replicatee 5 ['a']

--Exercise 3: Define a function thatselects the kth element of a list. Define a function that

selKth ::  [a] -> Int -> [a] 
selKth  xs 0 = head xs 
selKth  xs n  = selKth $ tail xs (n-1)

test3 :: [Char]
test3 = selKth "Haskell" 5

--Exercise 4: Define a function that decides if a value is an element of a list.

elemm ::  Eq a => a -> [a] -> Bool
elemm _ [] = False
elemm x (z:zs) = ( x == z ) && elemm x zs 

test4 :: Bool
test4 = elemm 'l' "Haskell"

main :: IO()
main = do
    print $ and [test1,test2,test3,test4]