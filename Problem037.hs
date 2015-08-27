module Problem037 (answer) where

import NumUtil (decompose, recompose)
import Primes (isPrime, primes)

answer :: Int
answer = sum $ take 11 $ filter isTruncatable (drop 4 primes)

isTruncatable p = fromLeft && fromRight
  where
    fromLeft = all (isPrime . recompose) (takeWhile ((>0) . length) (iterate tail $ decompose p))
    fromRight = all (isPrime . recompose) (takeWhile ((>0) . length) (iterate init $ decompose p))
