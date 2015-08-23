module Problem024 (answer) where

import Data.List (unfoldr)
import NumUtil (recompose)

answer :: Int
answer = recompose $ unfoldr choseDigit (1000000-1, [0..9])

choseDigit :: (Int, [Int]) -> Maybe (Int, (Int, [Int]))
choseDigit (_, []) = Nothing
choseDigit (target, digits)
  | target <= 0 = Just (head digits, (target, tail digits))
  | otherwise   = Just (digits !! d, (newTarget, newDigits))
    where
      f = fact $ length digits - 1
      d = target `div` f
      newTarget = target - d * f
      newDigits = pickIndex d digits


fact :: Int -> Int
fact 0 = 1
fact n = product [1..n]

pickIndex :: Int -> [a] -> [a]
pickIndex _ [] = undefined
pickIndex i (x:xs)
  | i == 0 = xs
  | otherwise = x : pickIndex (i-1) xs
