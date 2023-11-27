in_range :: Integer -> Integer -> Integer -> Bool
in_range min max x = x >= min && x <= max 

main :: IO()
main = do
    let result = in_range 1 10 5
    putStrLn $ "result: " ++ show result