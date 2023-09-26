butLast :: [a] -> a 
butLast [b ,  ]= b
butLast (:xs) = butLast xs 

butLast' :: [a] -> a 
butLast' x =  reverse x !! 1 

butLast'' :: [a] -> a 
butLast'' x = (!!) (reverse x) 1


--

butLast :: [a] -> a 
butLast [b , _ ]= b
butLast (_:xs) = butLast xs 

butLast' :: [a] -> a 
butLast' x =  reverse x !! 1 

butLast'' :: [a] -> a 
butLast'' x = (!!) (reverse x) 1 

---

last'' = head .drop 1 . reverse 

--

secondToLast :: [a] -> a
secondToLast [] = error "list is empty"
secondToLast [x] = error "only one element in the list"
secondToLast (x:[_]) = x
secondToLast (_:xs) = secondToLast xs

main = do print (secondToLast [1..4])

--

previous :: [a] -> a
previous x | length x < 2 = error "None elements"
previous x | length x == 2 = head x
previous (x:xs) = previous xs