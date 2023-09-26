-- guards , if else
-- otherwise , constant always true


fac n
    | n <= 1 = 1
    | otherwise = n * fac (n-1)

main ::IO()
main = do
    let result = fac 5
    putStrLn $ "result : " ++ show result