import System.Win32 (xBUTTON1)
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

--fn that converts an int of minutes into Time of (h,m)
toTime :: Int -> Time
toTime x = ( x `div` 60, x `mod` 60 )

--fn to test using time type in a comparaison of min
testfn :: Time -> Int -> Bool
testfn x n =
    n < tomin x
    
--fn to test using a list of time type in a comparaison of min

testfnL :: [Time] -> Int -> Bool
testfnL x n =
    n < tomin (x!!1)

--fn to subtract el1 and el2 of a list of time types into a Int
el1 :: [Time] -> Int
el1 x = 
    tomin (x!!1) - tomin (head x)


--fn to subtract el1 and el2 of a list of time types into a tuple Int,Int

el3 :: [Time] -> Time
el3 x = toTime(el1 x)



goToBed :: [Time] -> Int -> Time
goToBed (x:xs) n | xs == [] = x
                | (tomin (xs!!0)) - (tomin x) >= n = x
                
                | otherwise = goToBed xs n
-- goToBed

-- goToBed [(h,m)] n = (h,m) if time12
-- goToBed (x:y) n =  n < (tomin y - tomin x)
--     where 
--         tomin a = 60*fst + snd
    -- goToBed(drop 1 (x:y)) n

    
    --create fn that converts the in list into minutes based on midnight
    -- 1.0 <n 

    -- input to minutes : (60*fst(x)+snd(x),0)
    -- difference between input and input -1
    -- recursive step MB : goToBed(drop 1 (x:y)) n

    -- go

testB1 = goToBed [(0, 00), (1,00), (3,13), (5,00), (7,43),(15,05)] 140 == (5, 0)
testB2 = goToBed [(0, 00), (1,00), (3,13), (5,00), (7,43),(15,05)] 240 == (7, 43)
testB3 = goToBed [(0, 00), (0, 55)] 60 == (0, 55)
testB4 = goToBed [(0, 00), (0, 55)] 45 == (0, 0)


--Problem C----------------------------------------------------------

-- hanoi :: Int -> Int
-- hanoi 1 = 1