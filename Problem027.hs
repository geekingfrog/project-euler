module Problem027 (answer) where

import Primes (isPrime)
import Data.List (maximumBy)
import Data.Function (on)

answer :: Int
answer = (\(a, b, _) -> a*b) $ maximumBy (compare `on` third) searchSpace
  where
    searchSpace = [(a, b, nPrimes a b) | a <- [-999..999], b <- [-999..999]]
    third (_, _, a) = a

nPrimes :: Int -> Int -> Int
nPrimes a b = length $ takeWhile isPrime (quads a b)

quads :: Int -> Int -> [Int]
quads a b = filter (>0) $ map (\x -> x*x + a*x + b) [0..]
