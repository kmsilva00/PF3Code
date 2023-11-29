percentage :: Int -> Int -> Float
-- that takes two Int values, x and y, and returns the percentage of x with respect to y. 
-- Remember to use fromIntegral to convert Int to Float before performing the division.
percentage x y = (fromIntegral x / fromIntegral y)

circleArea :: Int -> Float
-- that takes an Int radius and returns the area of the circle. Use the formula pi * r^2 for the area. 
-- Remember to convert the radius to Float before squaring.
circleArea r =  3.14 * fromIntegral r * fromIntegral r

average :: [Int] -> Float
average [] = 0.0
average xs = (fromIntegral (sumTerms xs) / fromIntegral (length xs))
    where
        sumTerms :: [Int] -> Int
        sumTerms [] = 0
        sumTerms (x:xs) = x + sumTerms xs

        
        
fahrenheitToCelsius :: Int -> Float
-- Use the formula (f - 32) * 5/9 to convert Fahrenheit to Celsius
fahrenheitToCelsius n = fromIntegral(n-32) * 5/9

sphereVolume :: Int -> Float
-- formula 4/3 * pi * r^3 for the volume
sphereVolume n = 4/3 * pi * fromIntegral n^3
