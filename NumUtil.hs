module NumUtil (
  decompose
) where

import Data.List (unfoldr)

-- base10 decomposition
decompose :: (Integral a, Num b) => a -> [b]
decompose n = reverse $ unfoldr decompose' n
  where
    decompose' 0 = Nothing
    decompose' n = Just (fromIntegral $ n `mod` 10, fromIntegral $ n `div` 10)
