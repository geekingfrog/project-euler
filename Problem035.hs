module Problem035 (answer) where

import Primes (primes, isPrime)
import NumUtil (decompose, recompose)

answer :: Int
answer = length . (filter isCircularPrime) $ (takeWhile (<1000000) primes)

isCircularPrime :: Int -> Bool
isCircularPrime p = all (isPrime . recompose) (tail . circularPermutation $ decompose p)

circularPermutation :: [a] -> [[a]]
circularPermutation n = go n (length n)
  where
    go _ 0 = []
    go [] _ = []
    go l@(x:xs) k = l : go (xs ++ [x]) (k-1)
