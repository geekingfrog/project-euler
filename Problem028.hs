module Problem028 (answer) where

answer :: Int
answer = 1 + sum (take ((s-1)*2) $ pick [2..])
  where s = 1001

-- pick number on diagonals means taking 1 number every 2n
-- n ranging from 0 to (s-1)/2 with s the side of the square
pick :: [Int] -> [Int]
pick = go 1 1 4
  where
    go _ _ _ [] = undefined
    go n nRemain totalRemain (x:xs)
      | totalRemain == 0 = go (n+1) (2*n) 4 xs
      | nRemain == 0 = x : go n (2*n-1) (totalRemain-1) xs
      | otherwise = go n (nRemain-1) totalRemain xs
