factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

factorial' :: Int -> Int
factorial' n = product [1..n]

-- Codewars
-- https://www.codewars.com/kata/51f2d1cafc9c0f745c00037d/train/haskell
solution :: String -> String -> Bool
solution a b = drop (length a - length b) a == b 


-- Codewars
-- https://www.codewars.com/kata/56747fd5cb988479af000028/train/haskell
getMiddle :: String -> String
getMiddle s
    | length s `mod` 2 == 0 = [s !! ((length s `div` 2)-1)] ++ [s !! ((length s `div` 2))]
    | otherwise = [head $ drop (length s `div` 2) s]