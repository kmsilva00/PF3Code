factorial n = product [1..n]

main :: IO ()
main = do
  let result = factorial 5 -- Compute factorial of 5 (or any other number)
  putStrLn ("Factorial2: " ++ show result) -- Print the result
