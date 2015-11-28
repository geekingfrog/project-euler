module Problem043 (answer) where

import NumUtil (decompose, recompose, genPanDigitNumbers)

answer :: Int
answer = sum $ filter verify (genPanDigitNumbers 0 9)

verify :: Int -> Bool
verify n = and properties
  where properties = [prop1 ns, prop2 ns, prop3 ns] ++ map genProp args
        args = zip3 (repeat ns) [4..] [7,11,13,17]
        ns = decompose n

genProp :: ([Int], Int, Int) -> Bool
genProp (ns, offset, divisor) = (n' `mod` divisor) == 0
  where n' = recompose (take 3 . drop offset $ ns)

prop1 ns = ((ns !! 3) `mod` 2) == 0
prop2 ns = (sum (take 3 . drop 2 $ ns) `mod` 3) == 0
prop3 ns = (d == 0) || (d == 5)
  where d = ns !! 5
