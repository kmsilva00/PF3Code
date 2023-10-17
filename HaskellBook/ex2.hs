sumy [] = 0
sumy [x] = x
sumy (x:xs) = x + sumy xs