module Problem010 (answer) where

import Primes (primes)

answer :: Int
answer = sum $ takeWhile (<2000000) primes
