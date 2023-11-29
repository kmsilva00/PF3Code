last' :: [a] -> a
last' [] = error "list is empty"
last' [x] = x
last' (x:xs) = last' xs

main = do print (last' [1..4])

las :: [a] -> a
las (x:xs) | null xs  = x
           | otherwise = las xs

main :: IO ()
main = do
    print $ las [1,2,3]

lasti :: [a] -> a 
lasti [] = error("no")
lasti (x:[]) = x
lasti (x:xs) = lasti xs and drop ((length [1..10]) - 1) [1..10]


lastWitououtLast :: [a] -> a
lastWitououtLast [] = error "The list is empty"
lastWitououtLast [y] = y 
lastWitououtLast (_:ys) = lastWitououtLast ys


last' [] = error("empty")
last' a = a !! max 0 ((length a) - 1)


las00 = head . reverse