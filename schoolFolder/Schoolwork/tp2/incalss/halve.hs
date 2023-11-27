halve :: [a] -> ([a], [a])
halve x = (take len x, drop len x) where
    len = (length x) `div` 2

