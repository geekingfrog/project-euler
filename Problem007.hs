module Problem007 (answer) where

import Primes (primes)

answer :: Int
answer = primes !! 10000
