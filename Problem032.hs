module Problem032 (answer) where

import Data.List (sort, nub)

-- 3234 mostly randomly chosen
answer :: Int
answer = (sum . nub) [i*j | i <- [2..3234], j <- [1..i-1], isHit i j]

isHit :: Int -> Int -> Bool
isHit i j = ((== "123456789") . sort) $ (show i) ++ (show j) ++ (show (i*j))
