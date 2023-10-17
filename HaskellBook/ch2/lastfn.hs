-- last [1,2,3,4,5] = 5

last' :: [int] -> int 
last' xs = xs !! ( length xs - 1)


last1' :: [int] -> int
last1' xs = head ( reverse (xs))