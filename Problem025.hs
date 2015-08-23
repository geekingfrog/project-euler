module Problem025 (answer) where

import NumUtil (fibs)

answer :: Integer
answer = (fst . head) $ dropWhile ((<1000) . (length . show) . snd) (zip [1..] fibs)
