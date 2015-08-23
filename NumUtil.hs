module NumUtil (
  decompose,
  recompose
) where

import Data.List (unfoldr)

-- base10 decomposition
decompose :: (Integral a, Num b) => a -> [b]
decompose n = reverse $ unfoldr decompose' n
  where
    decompose' 0 = Nothing
    decompose' n = Just (fromIntegral $ n `mod` 10, fromIntegral $ n `div` 10)

-- the dual of decompose. recompose . decompose = id
recompose :: Num a => [a] -> a
recompose = foldl (\x y -> 10*x + y) 0
