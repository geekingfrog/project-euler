module Problem030 (answer) where

import NumUtil (decompose)

-- n * 9^5 < 9..9 (n digits) for n = 6

answer :: Int
answer = sum $ filter (\x -> x == sumOfPower 5 x) [2..999999]

-- power n p = n^p
power :: Int -> Int -> Int
power n p = floor (fromIntegral n ** fromIntegral p)

sumOfPower p = sum . map (`power` p) . decompose
