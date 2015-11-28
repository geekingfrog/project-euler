module NumUtil (
  decompose,
  recompose,
  fibs,
  toBase,
  genPanDigitNumbers
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

fibs :: Num a => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

toBase :: Integral a => a -> a -> [a]
toBase base = unfoldr d
  where
    d 0 = Nothing
    d x = Just (x `mod` base, x `div` base)


-- generate n pandigital numbers (start to end)
genPanDigitNumbers :: Int -> Int -> [Int]
genPanDigitNumbers start end = map recompose (go [start..end] [[]])
  where
    go :: [Int] -> [[Int]] -> [[Int]]
    go [] acc = acc
    go digits acc = concat [go ds (map (\l -> d:l) acc) | (d, ds) <- split digits]

split :: [a] -> [(a, [a])]
split [] = []
split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]
