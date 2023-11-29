-- https://www.codewars.com/kata/55847fcd3e7dadc9f800005f

data Tree = Node { val :: Int, left, right :: Maybe Tree }

compare' Nothing Nothing = True
compare' _ Nothing = False
compare' Nothing _ = False
compare' (Just x) (Just y)
    | val x == val y = compare' (left x) (left y) && compare' (right x) (right y)
    | otherwise = False

----------

compare' ::Maybe Tree->Maybe Tree->Bool
compare' Nothing Nothing = True
compare'  Nothing = False
compare' Nothing  = False
compare' (Just t1) (Just t2) = (val t1 == val t2) && (compare' (left t1) (left t2)) && (compare' (right t1) (right t2))