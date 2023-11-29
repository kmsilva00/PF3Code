feb :: Int -> Int

feb 0 = 1
feb 1 = 1
-- feb 2 = feb 1 + feb 2
feb n = feb (n-1) + feb (n-2)

-- main ::IO()
-- main = do
--     let result = feb 3
--     putStrLn $ "result : " ++ show result

--don't work
fib = gen 1 2 where gen x y = x : (gen y (x + y))

