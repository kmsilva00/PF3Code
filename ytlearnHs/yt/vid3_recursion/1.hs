--no loops only recursion

fac n = 
    if n <= 1 then
        1
    else
        n*fac(n-1)

main ::IO()
main = do
    let result = fac 5
    putStrLn $ "result : " ++ show result