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

stepsToOne :: [(Int,Int)]
stepsToOne = [(n,countCollatzSteps n)| n <-[1..]]

--Problem B----------------------------------------------------------
type Time = (Int, Int)
--tomin fn that converts the Time tuple into overall minutes
tomin :: Time -> Int
tomin x =  60 * fst x + snd x

goToBed :: [Time] -> Int -> Time
goToBed (x:xs) n | xs == [] = x
                | tomin (xs!!0) - (tomin x) >= n = x
                
                | otherwise = goToBed xs n

testB1 = goToBed [(0, 00), (1,00), (3,13), (5,00), (7,43),(15,05)] 140 == (5, 0)
testB2 = goToBed [(0, 00), (1,00), (3,13), (5,00), (7,43),(15,05)] 240 == (7, 43)
testB3 = goToBed [(0, 00), (0, 55)] 60 == (0, 55)
testB4 = goToBed [(0, 00), (0, 55)] 45 == (0, 0)


--Problem C----------------------------------------------------------

<<<<<<< HEAD
--
{- 
I will use a diagram to explain why a tower of hanoi requires 2^n - 1 steps.(*)
=======
-- Computes the number of steps needed to move the whole stack of n discs from one rod to another

{-

I use the formula that describes the minimal amount of steps to complete a tower of hanoi with n disks => 2^n - 1
I will try to prove this :

I will use a diagram to explain why a tower of hanoi requires 2^n - 1 steps.
>>>>>>> 21fadae6b73234f064295cb32ef23c8416c7e1d7

Nomenclature :

n refers to the number of disks
We will name the pegs A, B, C
<<<<<<< HEAD
the order in which the pegs are written matters; 
    from left to right, they indicate the position of the smallest to biggest disk
    example : AAA would mean smallest,medium and large disk are on peg at position A
              ABC would mean smallest disk in A, medium disk in B, largest disk in C 

The fastest methodology to solve the puzzle with n=1 would be :
A -> B

=> 0 step, nothing has to be done


The fastest methodology to solve the puzzle with n=2 would be :
=======
the order in which the pegs are written in my explanation DOES matter; 
    from left to right, they indicate the position of the smallest to biggest disk
    example : AAA would mean smallest,medium and large disk are on peg at position A
              ABC would mean smallest disk in peg A, the medium disk in peg B, largest disk in peg C 

-- solving hannoi puzzle from n= 0 to n=3

The fastest* methodology to solve the puzzle with n=0 would be :
=> well, nothing (0). There is nothing to do


The fastest* methodology to solve the puzzle with n=1 would be :
A -> B

=> 1 step, simply move the disk to another peg


The fastest* methodology to solve the puzzle with n=2 would be :
>>>>>>> 21fadae6b73234f064295cb32ef23c8416c7e1d7

AA -> BA -> BC -> CC 
Alternativly
AA -> CA -> CB -> CC 

<<<<<<< HEAD
=> 3 steps 
    ( AA is already given state so AA is not included as a step as nothing needs to happen for the state to be AA)

The fastest methodology to solve the puzzle with n=3 would be :
=======
=> for n = 2, 3 steps are needed to solve the puzzle
    ( AA is already given state so AA is not included as a step as nothing needs to happen for the state to be AA)

The fastest* methodology to solve the puzzle with n=3 would be :
>>>>>>> 21fadae6b73234f064295cb32ef23c8416c7e1d7

AAA -> BAA -> BCA -> CCA -> CCB -> ACB -> ABB -> BBB 
Alternativly
AAA -> CAA ->CBA -> BBA -> BBC -> ABC -> ACC -> CCC

Of course there are other solutions, but these are the less step intensive ones for n = 3

we can see that the number of steps for n = 3, is 8 = 2^3. 

<<<<<<< HEAD
However, again, the position AAA is always given as a start point so we must substract 1

n=  1 | steps = 0
n =

=> 2^n - 1


-}



=======
However, again, the position AAA is always given as a start point for n > 0. 

So we must substract 1 from the final result of the number of steps, as to get to the start state (i.e. AAA), no step must be done. Therefore:

steps for n = 3
    => 2^3 - 1 = 8-1 = 7

n=  0 | steps = 0
n = 1 | steps = 1
n=  2 | steps = 3
n = 3 | steps = 7

A pattern can be observed:

=> 2^n - 1

This result also makes sense. 
For every new disk, to solve the puzzle the n-1 disk must be solved 2 times. The largest piece introduced (n-th disk) can only move when the n-1 disks are deplaced as per the rules of the puzzle.

-}

hanoi :: Int -> Int
hanoi n = 2^n-1

-- testC1 = hanoi 3 == 7
-- testC2 = hanoi 14 == 16383
>>>>>>> 21fadae6b73234f064295cb32ef23c8416c7e1d7
