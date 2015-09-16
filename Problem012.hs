module Problem012 (answer) where

import Primes (factorize)

answer :: Int
answer = head $ dropWhile (\x -> dn x <= 500) triangulars

-- for a number n = prod(p_i^k_i) where p_i^k_i is its prime factorization
-- the number of divisor d(n) is prod(k_i + 1)
dn :: Int -> Int
dn n = product $ map ((+1) . snd) (aggregate (factorize n))

triangulars = [n * (n+1) `div` 2 | n <- [1..]]

-- count the number of items from a given sorted list
-- aggregate [2,2,3,5,5] -> [(2, 2), (3, 1), (5, 2)]
aggregate :: [Int] -> [(Int, Int)]
aggregate = foldl aggregate' []
  where
    aggregate' [] n = [(n, 1)]
    aggregate' acc@((x, count):xs) n =
      if x == n then (x, count+1):xs
      else (n, 1):acc
