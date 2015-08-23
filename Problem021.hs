module Problem021 (answer) where

import Primes (properDivisors)

answer :: Int
answer = sum [i | i <- [2..10000], isAmicable i]

isAmicable :: Int -> Bool
isAmicable n = let dn = d n
  in (n /= dn) && (d dn == n)

-- sum of proper divisor
d :: Int -> Int
d n = sum $ properDivisors n
