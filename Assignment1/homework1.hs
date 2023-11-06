import System.Win32 (xBUTTON1)
import Distribution.Simple.Test (test)
import qualified GHC.TypeLits as steps
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

--
{- 
I will use a diagram to explain why a tower of hanoi requires 2^n - 1 steps.(*)

Nomenclature :

n refers to the number of disks
We will name the pegs A, B, C
the order in which the pegs are written matters; 
    from left to right, they indicate the position of the smallest to biggest disk
    example : AAA would mean smallest,medium and large disk are on peg at position A
              ABC would mean smallest disk in A, medium disk in B, largest disk in C 

The fastest methodology to solve the puzzle with n=1 would be :
A -> B

=> 0 step, nothing has to be done


The fastest methodology to solve the puzzle with n=2 would be :

AA -> BA -> BC -> CC 
Alternativly
AA -> CA -> CB -> CC 

=> 3 steps 
    ( AA is already given state so AA is not included as a step as nothing needs to happen for the state to be AA)

The fastest methodology to solve the puzzle with n=3 would be :

AAA -> BAA -> BCA -> CCA -> CCB -> ACB -> ABB -> BBB 
Alternativly
AAA -> CAA ->CBA -> BBA -> BBC -> ABC -> ACC -> CCC

Of course there are other solutions, but these are the less step intensive ones for n = 3

we can see that the number of steps for n = 3, is 8 = 2^3. 

However, again, the position AAA is always given as a start point so we must substract 1

n=  1 | steps = 0
n =

=> 2^n - 1


-}



