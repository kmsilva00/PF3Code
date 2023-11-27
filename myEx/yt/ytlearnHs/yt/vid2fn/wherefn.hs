-- where you have the final result of fn, and say where cerain variables are defined as certains expressions

in_range min max x = ilb && iub
    where
        ilb = min <= x
        iub = max >= x

main :: IO()
main = do
    let result = in_range 1 2 3
    putStrLn $ "result : " ++ show result