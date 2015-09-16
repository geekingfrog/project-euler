module Problem038 (answer) where

import NumUtil (decompose)
import Data.List

answer :: Int
answer = multPandigital $ last candidates
  where
    candidates = filter (isPandigital . multPandigital) $ filter noDupDigit [1..98765]

multPandigital n = head $ dropWhile not9digit (concatMultiple n)
  where not9digit = (<9) . length . decompose

concatMultiple n = scanl1 concatNumber $ iterate (+n) n

concatNumber :: Integral a => a -> a -> a
concatNumber a b = a*power + b
  where
    power = 10 ^ length (decompose b)

isPandigital n = [1 .. 9] == sort (decompose n)

noDupDigit n = length dn == length (nub dn)
  where dn = decompose n
