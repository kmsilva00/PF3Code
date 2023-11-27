--https://www.codewars.com/kata/57a852c353ba334961001480/train/haskell

module PersonSaysHi where

data Person = Person { firstName :: String, lastName :: String }
--data Person = Person String String 

sayHi :: Person -> String
sayHi (Person a b) = "Hi, i'am " ++ a ++ " " ++ b ++ " and it is nice to meet You."

-----------
data Person = Person {
    firstName :: String,
    lastName :: String
}

sayHi :: Person -> String
sayHi person = "Hi, I am " ++ firstName person ++ " " ++ lastName person ++ " adn it is nice to meet you."
---

module PersonSaysHi where

data Person = Person { firstName :: String, lastName :: String }
sayHi :: Person -> String
sayHi p = "Hi, i'am "++firstName p++" "++lastName p++" and it is nice to meet You."