module Problem039 (answer) where

import Data.List (maximumBy)
import Data.Function (on)

answer :: Int
answer = fst $ maximumBy (compare `on` snd) solutionSpace
  where
    solutionSpace = zip [3..1000] $ fmap (length . rightTriangles) [3..1000]

rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles perimeter = [(a, b, truncate c) |
  a <- [1..perimeter-1],
  b <- [a..perimeter],
  let c = sqrt . fromIntegral $ a*a + b*b,
  isInt c,
  a + b + truncate c == perimeter
  ]

isInt :: Float -> Bool
isInt a = fromIntegral(round a) == a
