module Problem029 (answer) where

import Data.List (nub)

answer :: Int
answer = length $ nub [a**b | a <- [2..100], b <- [2..100]]
