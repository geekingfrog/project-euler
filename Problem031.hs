module Problem031 (answer) where

import Control.Monad.Trans.State
import Control.Monad (guard)

answer :: Int
answer = length $ count 200

count target = flip evalStateT target $ do
  res <- mapM (StateT . pick) [1,2,5,10,20,50,100,200]
  s <- get
  guard $ s == 0  -- we must reach the given target
  return res

pick :: Int -> Int -> [(Int, Int)]
pick c target = [(n, target-n*c) | n <- [0..target `div` c]]
