module Problem041 (answer) where

import NumUtil (genPanDigitNumbers)
import Primes (isPrime)

answer :: Int
answer = maximum $ filter isPrime (concatMap (uncurry genPanDigitNumbers) range)
  where range = zip (repeat 1) [2..9]
