module Problem002 (answer) where

answer :: Int
answer = sum $ filter even $ takeWhile (<4000000) fibs

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
