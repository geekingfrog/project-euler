module Problem004 (answer) where

import Data.List (unfoldr)

answer :: Int
answer = maximum [x*y | x <- [100..999], y <- [100..999], isPalindrome (x*y)]

isPalindrome :: Int -> Bool
isPalindrome n = ns == reverse ns
  where ns = decompose n

decompose :: Int -> [Int]
decompose n = unfoldr decompose' n
  where
    decompose' 0 = Nothing
    decompose' n = Just (n `mod` 10, n `div` 10)
