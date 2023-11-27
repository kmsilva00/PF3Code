solution :: String -> String -> Bool
solution str1 str2 = startsWith (reverse str1) (reverse str2)

startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith (x:xs) (y:ys) | x == y = startsWith xs ys
                         | otherwise = False


solution' :: Eq a => [a] -> [a] -> Bool
solution' a b = (drop (length a - length b) a) == b

main :: IO ()
main = do
  let result = solution "abcd" "a"
  putStrLn $ "Result: " ++ show result



--