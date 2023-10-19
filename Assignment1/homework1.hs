import Distribution.Simple.Test (test)
--1------------------------------------------------------------------
collatzStep :: Int -> Int
collatzStep n = if even n then
    n`div`2
    else 
    3*n+1

--2------------------------------------------------------------------
countCollatzSteps :: Int -> Int
countCollatzSteps 1 = 0
countCollatzSteps n = 1 + countCollatzSteps (collatzStep n)

--3------------------------------------------------------------------
-- stepsToOnen :: Int -> [(Int,Int)]
-- stepsToOnen n = [(n,(countCollatzSteps n))]

stepsToOne :: [(Int,Int)]
stepsToOne = [(n,countCollatzSteps n)| n <-[1..]]

--Problem B----------------------------------------------------------
type Time = (Int, Int)
--tomin fn that converts the Time tuple into overall minutes
tomin :: Time -> Int
tomin x =  60 * fst x + snd x

-- --fn that converts an int of minutes into Time of (h,m)
-- toTime :: Int -> Time
-- toTime x = ( x `div` 60, x `mod` 60 )

-- --fn to test using time type in a comparaison of min
-- testfn :: Time -> Int -> Bool
-- testfn x n =
--     n < tomin x
    
-- --fn to test using a list of time type in a comparaison of min

-- testfnL :: [Time] -> Int -> Bool
-- testfnL x n =
--     n < tomin (x!!1)

-- --fn to subtract el1 and el2 of a list of time types into a Int
-- el1 :: [Time] -> Int
-- el1 x = 
--     tomin (x!!1) - tomin (head x)


-- fn to subtract el1 and el2 of a list of time types into a tuple Int,Int

-- el3 :: [Time] -> Time
-- el3 x = toTime(el1 x)

goToBed :: [Time] -> Int -> Time
goToBed (x:xs) n | xs == [] = x
                | tomin (xs!!0) - (tomin x) >= n = x
                
                | otherwise = goToBed xs n

testB1 = goToBed [(0, 00), (1,00), (3,13), (5,00), (7,43),(15,05)] 140 == (5, 0)
testB2 = goToBed [(0, 00), (1,00), (3,13), (5,00), (7,43),(15,05)] 240 == (7, 43)
testB3 = goToBed [(0, 00), (0, 55)] 60 == (0, 55)
testB4 = goToBed [(0, 00), (0, 55)] 45 == (0, 0)


--Problem C----------------------------------------------------------

-- Computes the number of steps needed to move the whole stack of n discs from one rod to another

{-

I use the formula that describes the minimal amount of steps to complete a tower of hanoi with n disks => 2^n - 1
I will try to prove this :

When simply one ring is available ( n = 1 ), and we have 3 pegs, we simply need 1 move to displace the ring from Peg 1 ( P1 ) to Peg 3 ( P3 )
for 2 rings R1, R" and 3 Pegs P1 P2 P3 : 

-}

hanoi :: Int -> Int
hanoi n = 2^n-1

testC1 = hanoi 3 == 7
testC2 = hanoi 14 == 16383