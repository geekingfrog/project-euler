module Problem002 (answer) where

import NumUtil (fibs)

answer :: Int
answer = sum $ filter even $ takeWhile (<4000000) fibs
