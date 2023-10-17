module Codewars.G964.Getmiddle where

getMiddle :: String -> String
getMiddle [] = []
getMiddle [x] = [x]
getMiddle [x,y] = [x,y]
getMiddle x = getMiddle (drop 1 (init x))

main :: IO()
main = do
    let result = getMiddle "abd"
    putStrLn $ "Result: " ++ show result