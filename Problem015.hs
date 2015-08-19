module Problem015 (answer) where

answer :: Int
answer = numberOfPaths 20 20

-- numberOfPaths (x,y): number of lattice paths from (0,0) to (x,y)
numberOfPaths :: Int -> Int -> Int
numberOfPaths x y = last $ accumulateRows y (replicate (x+1) 1)
  -- where
accumulateRows 0 ps = ps
accumulateRows n ps = accumulateRows (n-1) (scanl1 (+) ps)
