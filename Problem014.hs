module Problem014 (answer) where

import Control.Arrow ((&&&))
import Data.List
import Data.Function (on)

answer :: Int
answer = fst $ maximumBy (compare `on` snd) chains
  where
    chains = map (id &&& (length . collatz)) [1..1000000]

collatz :: Int -> [Int]
collatz seed = takeWhile (/= 1) $ iterate go seed
  where
    go n
      | even n = n `div` 2
      | otherwise = 3 * n + 1
