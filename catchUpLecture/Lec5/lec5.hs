type Pair a = (a,a)

addP :: Num a => Pair a -> a
addP (x,y) = x+y

type Coordinate = (Int,Int)
data Direction = North | South | East | West

moveOne :: Direction -> Coordinate -> Coordinate
moveOne North (x,y) = (x,y+1)
moveOne South (x,y) = (x,y-1)
moveOne East (x,y) = (x+1,y)
moveOne West (x,y) = (x-1,y)

data Shape = Circle Float | Rectangle Float Float
    deriving (Show, Eq)

area :: Shape -> Float
area ( Circle r )= pi * r^2
area (Rectangle x y ) = x * y  
