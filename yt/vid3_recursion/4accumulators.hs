fac n = aux n 1
    where
        aux n acc
            | n <= 1 = acc
            | otherwise = aux (n-1) (n*acc)

main ::IO()
main = do
    let result = fac 5
    putStrLn $ "result : " ++ show result


-- fac fn is the result of aux fn
-- aux fn takes n and another argument acc
-- ?????????????????