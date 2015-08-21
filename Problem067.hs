{-# LANGUAGE BangPatterns #-}

module Problem067 (answer) where
import qualified Problem018 as P18

answer :: IO Int
answer = do
  content <- readFile "./data/67.txt"
  let triangle = readTriangle content
  return $ P18.shortestPath 1 1 triangle

readTriangle :: String -> [[Int]]
readTriangle content = map (\l -> map read (words l)) (lines content)
