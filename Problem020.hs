module Problem020 (answer) where

import Data.List (foldl')
import NumUtil (decompose)

answer :: Int
answer = sum . decompose . fact $ 100

fact :: Integer -> Integer
fact n = foldl' (*) 1 [1..n]
