module Problem041 (answer) where

import NumUtil (recompose)
import Primes (isPrime)

answer :: Int
answer = maximum $ filter isPrime (concatMap genPanDigitNumbers [2..9])

-- generate n pandigital numbers
genPanDigitNumbers :: Int -> [Int]
genPanDigitNumbers n = map recompose (go [1..n] [[]])
  where
    go :: [Int] -> [[Int]] -> [[Int]]
    go [] acc = acc
    go digits acc = concat [go ds (map (\l -> d:l) acc) | (d, ds) <- split digits]

split :: [a] -> [(a, [a])]
split [] = []
split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]
