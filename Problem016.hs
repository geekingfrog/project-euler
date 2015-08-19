module Problem016 (answer) where

import Data.List (unfoldr)

answer :: Int
answer = sum $ digitize $ (iterate (*2) 1) !! 1000

digitize :: Integer -> [Int]
digitize n = unfoldr digitize' n
  where
    digitize' 0 = Nothing
    digitize' n = Just (fromIntegral $ n `mod` 10, fromIntegral $ n `div` 10)
