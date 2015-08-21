module Problem018 (answer) where

import Data.Array

-- See http://jelv.is/blog/Lazy-Dynamic-Programming/
-- for how to use lazyness with dynamic programming

answer :: Int
answer = shortestPath triangle 1 1

shortestPath :: [[Int]] -> Int -> Int -> Int
shortestPath costs i j = path i j
  where
    costs' = convert costs
    (n, _) = (snd . bounds) costs'
    path i' j'
      | i' == n = costs' ! (n, j')
      | otherwise = (costs' ! (i',j')) + max (cs ! (i'+1, j')) (cs ! (i'+1, j'+1))
    cs = listArray (bounds costs') [path i j | (i,j) <- range $ bounds costs']

-- function to add fake elements to a [[Int]] in order to build an array
expand :: Bounded a => [a] -> Int -> [a]
expand l n
  | n <= 0 = l
  | otherwise = expand (l ++ [maxBound]) (n-1)

-- Convert a rectangular [[Int]] into an array
convert :: [[Int]] -> Array (Int, Int) Int
convert ls = listArray ((1, 1), (l, l)) (concat expanded)
  where
    l = length . last $ ls
    expanded = map (\subL -> expand subL (l - length subL)) ls

triangle :: [[Int]]
triangle = [
  [75],
  [95, 64],
  [17, 47, 82],
  [18, 35, 87, 10],
  [20, 04, 82, 47, 65],
  [19, 01, 23, 75, 03, 34],
  [88, 02, 77, 73, 07, 63, 67],
  [99, 65, 04, 28, 06, 16, 70, 92],
  [41, 41, 26, 56, 83, 40, 80, 70, 33],
  [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
  [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
  [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
  [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
  [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
  [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
  ]

-- testTriangle :: [[Int]]
-- testTriangle = [
--   [3],
--   [7, 4],
--   [2, 4, 6],
--   [8, 5, 9, 3]
--   ]
