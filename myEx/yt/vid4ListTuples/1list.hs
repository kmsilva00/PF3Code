-- list of single type

a = 1:2:3:4:5:[]
b = [1,2,3,4,5]

asc :: Int -> Int -> [Int]
asc n m
    | m < n = []
    | m == n = [m]
    | m > n = n : asc (n+1) m

main ::IO()
main = do
    let result = asc 1 5
    putStrLn $ "result : " ++ show result
