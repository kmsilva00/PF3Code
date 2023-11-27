halve :: [a] -> ([a],[a])
halve l = (take halfl l, drop halfl l)
    where
        halfl = length l `div` 2


third :: [a] -> a
third (_:_:s:_) = s

third' :: [a] -> a
third' s = head $ tail $ tail s

third'' :: [a] -> a
third'' s = s!!2


safetail :: [ a ] -> [ a ]
safetail [] = []
safetail (x:xs) = xs

safetail' :: [ a ] -> [ a ]
safetail' xs = if null xs then [] else drop 1 xs 

safetail'' :: [a] -> [a]
safetail'' xs
    | null xs = []
    | otherwise = drop 1 xs


-- Write a function converting Bool in String by using:


-- a standard prelude function;
-- case expressions.

cbs :: Bool -> String

-- a conditional expression;
-- cbs b = if b :: Bool then show b else "False"

-- guarded equations;
-- cbs b
--     | b == True = "True"
--     | b == False = "False"

-- pattern matching;
-- cbs b = show b

-- a standard prelude function;
-- cbs b = show b

-- -- case expressions.
-- cbs x = case x of 
--     False -> "False"
--     True -> "True"