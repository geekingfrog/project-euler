module Problem003 (answer) where

import Primes (primes, factorize)

answer :: Int
answer = maximum $ factorize 600851475143
