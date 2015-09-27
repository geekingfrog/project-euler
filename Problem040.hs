module Problem040 (answer) where

import Data.Char

answer :: Int
answer = product dns
  where
    frac = concatMap show [0..]
    targets = take 7 $ iterate (*10) 1
    dns = map ((\c -> ord c - 48) . (frac !!)) targets
