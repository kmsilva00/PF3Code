module TP1_0 where

import Prelude hiding (null, elem, last, init, take, drop)

--EX Let's see standard prelude functions about lists [too early for reverse]
null :: [a] -> Bool
null [] = True
null (_:_) = False

elem :: (Eq a) => a -> [a] -> Bool
elem _ []       = False
elem x (y:ys)   = x==y || elem x ys

head  ::  [a] -> a
head (x:_)  =  x
head []  =  error("error message")

last :: [a] -> a
last [x]  =  x
last (_:xs) =  last xs
last []   =  error("error message")

init  ::  [a] -> [a]
init [x] =  []
init (x:xs) =  x : init xs
init [] =  error("error message")

tail :: [a] -> [a]
tail (_:xs) =  xs
tail []  =  error("error message")


take   :: Int -> [a] -> [a]
take 0 _  =  []
take _ [] =  []
take 1 [x]= []
take n (x:xs) =  x : take (n-1) xs

drop :: Int -> [a] -> [a] 
drop 0 xs =  xs
drop _ [] =  []
drop n (_:xs) =  drop (n-1) xs
