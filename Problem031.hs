module Problem031 (answer) where

import Data.Array

answer :: Integer
answer = count 200 [1, 2, 5, 10, 20, 50, 100, 200]

count :: Integer -> [Integer] -> Integer
count target coins = cs target maxCoin
  where
    maxCoin = maximum coins
    bounds = ((0,1), (target, maxCoin))
    counts = listArray bounds [cs i j | (i, j) <- range bounds]
    -- number of ways to count to i using coins of denomination j or lower
    cs t c
      | t==0 || c==1 = 1
      | t < c        = counts ! (t, nextCoin)
      | otherwise    = (counts ! (t, nextCoin)) + (counts ! ((t - c), c))
      where nextCoin = maximum $ filter (<c) coins


-- -- The solution with monad transformer is cute and very useful for
-- -- educational purpose, but it's still doing an exhaustive search
-- -- so it's very inefficient. It's kept below for posterity.
--
-- import Control.Monad.Trans.State
-- import Control.Monad (guard)
--
-- answer :: Int
-- answer = length $ count 200
--
-- count target = flip evalStateT target $ do
--   res <- mapM (StateT . pick) [1,2,5,10,20,50,100,200]
--   s <- get
--   guard $ s == 0  -- we must reach the given target
--   return res
--
-- pick :: Int -> Int -> [(Int, Int)]
-- pick c target = [(n, target-n*c) | n <- [0..target `div` c]]
