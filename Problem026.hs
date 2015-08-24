module Problem026 (answer) where

import Data.List (unfoldr, maximumBy)
import Data.Maybe (catMaybes)
import Data.Function (on)

answer :: Int
answer = fst $ maximumBy (compare `on` snd) $ map (\x -> (x, cycleLength x)) [2..999]

-- list of remainders in 1/n
remainders :: Int -> [Int]
remainders n = unfoldr unfolder 1
  where
    unfolder 0 = Nothing
    unfolder r = Just (r, (r*10) `rem` n)

cycleLength :: Int -> Int
cycleLength n = case cycles of
  [] -> 0
  _  -> 1 + maximum cycles
  where
    rs = concatTail . (take n) $ remainders n
    cycles = catMaybes $ map (\(x, xs) -> nextOcc x xs) rs

nextOcc :: (Eq a) => a -> [a] -> Maybe Int
nextOcc = go 0
  where
    go _ _ [] = Nothing
    go idx a (x:xs)
      | a == x = Just idx
      | otherwise = go (idx+1) a xs

concatTail :: [a] -> [(a, [a])]
concatTail [] = []
concatTail (x:xs) = (x, xs) : concatTail xs
