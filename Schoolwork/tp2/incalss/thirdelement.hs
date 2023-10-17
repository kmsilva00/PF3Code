third :: [a] -> a
third b | length b >= 3 = head (tail (tail b))


thirdB :: [a] -> a
thirdB cs = cs !! 2

thirdC :: [a] -> a
thirdC (_:_:x:_) = x

