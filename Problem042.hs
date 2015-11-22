module Problem042 (answer) where

import Data.List.Split (splitOn)
import Data.Char (ord)

answer :: IO Int
answer = do
  content <- readFile "./data/42.txt"
  let words = map (tail . init) (splitOn "," content)
  return $ length (filter isTriangleWord words)

wordValue :: String -> Int
wordValue w = sum $ map (\c -> ord c - ord 'A' + 1) w

isInt x = x == fromInteger (round x)

isTriangleWord w = isInt ((-1 + sqrt (1 + 8 * s)) / 2)
  where s = fromIntegral (wordValue w)
