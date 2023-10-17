qsorty [] = []
qsorty (x:xs) = qsorty biggery ++ [x] ++ qsorty smallery
    where
        biggery = [ a | a <- xs, a > x]
        smallery = [a | a <- xs, a <= x]
