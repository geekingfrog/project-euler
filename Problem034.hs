module Problem034 (answer) where

import NumUtil (decompose)

answer :: Int
answer = sum [i | i <- [3..999999], (sum $ map fact (decompose i)) == i]

fact 0 = 1
fact n = product [1..n]
