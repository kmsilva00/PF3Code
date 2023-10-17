-- bind result of an expression to a variable

in_range min max x =
    let i_l_b = min <= x
        i_u_b = max >= x
    in
    i_l_b && i_u_b

main :: IO()
main = do
    let result = in_range 4 5 6
    putStrLn $ "result : " ++ show result
