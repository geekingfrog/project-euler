module Problem016 (answer) where

import NumUtil (decompose)

answer :: Int
answer = sum $ decompose $ (iterate (*2) 1) !! 1000
