mysum [] = 0
mysum (x:xs) = x + sum xs

main :: IO ()
main = do
  let result = mysum [1,2,3,4,5]
  putStrLn $ "Result: " ++ show result