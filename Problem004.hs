module Problem004 (answer) where

import NumUtil (decompose)

answer :: Int
answer = maximum [x*y | x <- [100..999], y <- [100..999], isPalindrome (x*y)]

isPalindrome :: Int -> Bool
isPalindrome n = ns == reverse ns
  where ns = decompose n
