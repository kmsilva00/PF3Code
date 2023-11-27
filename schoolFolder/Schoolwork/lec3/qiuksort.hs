quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort l ++ [x] ++ quicksort r where
    l = filter (<x) xs
    r = filter (>=x) xs


quicksort' :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smallerThanX ++ [x] + quicksort biggerThanX
    where
        smallerThanX
        