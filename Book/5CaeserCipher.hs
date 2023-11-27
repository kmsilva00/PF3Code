import Data.Char

let2int :: Char -> Int
let2int a = ord a - ord 'a'

int2let :: Int -> Char
int2let a = chr ( a + 97)

shift :: Int -> Char -> Char
shift n c 
    | isLower c = int2let ((let2int c+n) `mod` 26)
    | otherwise = c