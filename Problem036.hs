module Problem036 (answer) where

import NumUtil (decompose, toBase)

answer :: Int
answer = sum [i | i <- [1..1000000], isPalindrome $ decompose i, isPalindrome $ (toBase 2 i)]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (reverse x) == x
