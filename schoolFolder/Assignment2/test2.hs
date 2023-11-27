myfn :: Int -> [Int] -> Int -> [Int] -> Int -> [[Int]]
myfn n (x5) pos (y:ys) 0 = [[]]
myfn n (x5) pos [] n1 = [[]]
myfn n (x5) pos (y:ys) n1 = if pos == (length x5)-1 then [[]] else (map (y:) (myfn n x5 (pos+1) z5 (n1-1))) ++ (myfn n x5 pos ys n1)
        where z5 =  [0..(n `div` (x5!!(pos+1)))] 