import System.Win32 (xBUTTON1)
last' :: [a] -> a
last' [] = error "list empty"
last' [x] = x
last' (_:xs) = last xs

main :: IO()
main = do
    print $ last' [1,2,3,4]