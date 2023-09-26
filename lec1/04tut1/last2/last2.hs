import System.Win32 (COORD(yPos))
last'' :: [a] -> a
last'' [] = error "error"
last'' [x] = error "error"
last'' [x,y] = x
last'' (x:xs) = last'' xs


main :: IO()
main = do
    print $ last'' [1,2,3,4]


-- yay my solution it's better to put [x,_]