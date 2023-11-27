double x = x*2

-- function that takes two numbers and multiplies each by two and then adds them together.
doublep2 x y = double x + double y

-- function that multiplies a number by 2 but only if that number is smaller than or equal to 100
doubleincon x = if x > 100
                then x
                else x*2


-- function to add one to every number that's produced in our previous function
doubleincon' x = (if x > 100 then x else x*2) + 1

-- fn deffinition in lower case ( also called definition)
conanO'Brien = "It's a-me, Conan O'Brien!"   

-- LISTS
-- gchi : let nlist = [1,2,3,4]
-- ghci : nlist
-- ghci [1,2,3,4]

-- Lists use the ++ operator for addition

nlist = [1,2,3,4]
nlist' = [5,4,3,2]
-- nlist + nlist' => [1,2,3,4,5,4,3,2]
-- ++ appends the lists

wlist = "Hello"
wlist' = "World"
-- wlist + wlist' = "HelloWorld"

-- con operator
flist = [2,3,4,5]
flist' = 1: flist
-- ghci> flist'
-- [1,2,3,4,5]

-- [1,2,3] = 1:2:3:[]

-- Getting element at index i. Use !! Operator
babyloniaS = "babylonia"
-- babyloniaS !! 6 = n


-- Can use all this in lists of lists of course

--If's in lists, first element is compared, then 2nd
-- ghci> [3,2,1] > [2,1,0]  
-- True  
-- ghci> [3,2,1] > [2,10,100]  
-- True  
-- ghci> [3,4,2] > [3,4]  
-- True  
-- ghci> [3,4,2] > [2,4]  
-- True  
-- ghci> [3,4,2] == [3,4,2]  
-- True  


-- head,tail || last, init
-- length

-- null[1,2,3] => False
-- null [] => True

-- reverse[1,2,3] = [3,2,1]

-- take and drop
-- ghci> take 3 [5,4,3,2,1]  
-- [5,4,3]  
-- ghci> take 1 [3,9,3]  
-- [3]  
-- ghci> take 5 [1,2]  
-- [1,2]  
-- ghci> take 0 [6,6,6]  
-- []  


-- ghci> drop 3 [8,4,2,1,5,6]  
-- [1,5,6]  
-- ghci> drop 0 [1,2,3,4]  
-- [1,2,3,4]  
-- ghci> drop 100 [1,2,3,4]  
-- []   


-- maximum, minimum

-- ghci> sum [5,2,1,6,3,2,5,7]  
-- 31  
-- ghci> product [6,2,1,2]  
-- 24  
-- ghci> product [1,2,5,6,7,9,2,0]  
-- 0   


-- ghci> 4 `elem` [3,4,5,6]  
-- True  
-- ghci> 10 `elem` [3,4,5,6]  
-- False  

-- Range in lists
-- ghci> [1..20]  
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]  
-- ghci> ['a'..'z']  
-- "abcdefghijklmnopqrstuvwxyz"  
-- ghci> ['K'..'Z']  
-- "KLMNOPQRSTUVWXYZ"   



-- ghci> [2,4..20]  
-- [2,4,6,8,10,12,14,16,18,20]  
-- ghci> [3,6..20]  
-- [3,6,9,12,15,18]   


-- take multiples of a number 
-- take 24 [13,26..]    


-- ghci> take 10 (cycle [1,2,3])  
-- [1,2,3,1,2,3,1,2,3,1]  
-- ghci> take 12 (cycle "LOL ")  
-- "LOL LOL LOL "   

-- ghci> take 10 (repeat 5)  
-- [5,5,5,5,5,5,5,5,5,5]  

-- replicate 3 10 returns [10,10,10].


-- [x*2 | x <- [1..10]]
-- [2,4,6,8,10,12,14,16,18,20]

-- [x*2 | x <- [1..10], x<=10]
-- [2,4,6,8,10,12,14,16,18,20]

-- [x | x  <- [50..100],x `mod` 7 == 3]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   


greenorblue xs = [if x `elem` xs then "green" else "blue"|x <- drop 5 xs]

-- multiple conditions
-- ghci> [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  
-- [10,11,12,14,16,17,18,20]  


-- ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]  
-- [16,20,22,40,50,55,80,100,110]   

asd xs xy= [x*y | x <- [1,2,3], y <- [4,5,6]]


-- ghci> let nouns = ["hobo","frog","pope"]  
-- ghci> let adjectives = ["lazy","grouchy","scheming"]  
-- ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]  
-- ["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",  
-- "grouchy pope","scheming hobo","scheming frog","scheming pope"]   


length' xs = sum[1 | _ <- xs]


removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

-- ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
-- ghci> [ [ x | x <- xs, even x ] | xs <- xxs]  
-- [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]  


-- ghci> fst (8,11)  
-- 8  
-- ghci> fst ("Wow", False)  
-- "Wow"  


-- ghci> snd (8,11)  
-- 11  
-- ghci> snd ("Wow", False)  
-- False  


-- ghci> zip [1,2,3,4,5] [5,5,5,5,5]  
-- [(1,5),(2,5),(3,5),(4,5),(5,5)]  
-- ghci> zip [1 .. 5] ["one", "two", "three", "four", "five"]  
-- [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]  



-- ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]  
-- [(5,"im"),(3,"a"),(2,"turtle")]  


-- ghci> zip [1..] ["apple", "orange", "cherry", "mango"]  
-- [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]  


-- ghci> let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]   

-- ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  
-- ghci> rightTriangles'  
-- [(6,8,10)]  