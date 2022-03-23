import Data.List

{-
    1. Function that takes an integer and doubles it. Think of an appropriate
    name for this function and give it a type signature.
-}
doubleInteger x = x + x 

{-
    2.Using your answer to the previous question, define a function which
    multiplies its input by 4.
-}
multiplyByFour x = x * 4

{-
    3.Define a function isInRange :: Integer -> Integer -> Integer -> Bool where isInRange
    a b c returns true if c is between the two inputs a and b (or equal to either of them) and
    false otherwise
-}
isInRange a b c = 
    if c > a && c < b
        then True
        else False
