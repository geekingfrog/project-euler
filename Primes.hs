module Primes (primes, factorize, properDivisors, isPrime) where

import Data.List (unfoldr, subsequences, nub)
import Data.PSQueue as PQ

primes = sieve [2..]

factorize :: Int -> [Int]
factorize = factorize' primes
  where
    factorize' primes = unfoldr (unfolder primes)
    unfolder ps x = case firstPrimeDivisor ps x of
          Nothing -> Nothing
          Just d -> Just (d, x `div` d)

-- list of all factors for a given number (not sorted)
factors :: Int -> [Int]
factors = nub . map product . subsequences . factorize

properDivisors :: Int -> [Int]
properDivisors = init . factors

firstPrimeDivisor :: Integral a => [a] -> a -> Maybe a
firstPrimeDivisor primes x = safeHead (filter (\p -> x `mod` p == 0) $ takeWhile (<= x) primes)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all (\p -> (n `mod` p) /= 0) prevPrimes
  where
    prevPrimes = takeWhile (\x -> x*x <= n) primes


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- For the algorithm, see
-- http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
-- I added some wrapper around the PQ module since its key, value is
-- inversed compared to the paper

type IteratorTable = PQ.PSQ [Int] Int

insertComposite :: Int -> [Int] -> IteratorTable -> IteratorTable
insertComposite n ns = PQ.insert ns n

minKey table = case PQ.findMin table of
  Nothing -> undefined
  Just binding -> prio binding

deleteMinAndInsert :: Int -> [Int] -> IteratorTable -> IteratorTable
deleteMinAndInsert n ns table = insertComposite n ns $ PQ.deleteMin table

findMinKeyValue :: IteratorTable -> (Int, [Int])
findMinKeyValue table = case PQ.findMin table of
  Nothing -> undefined
  Just binding -> (prio binding, key binding)

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve' xs (insertPrime x xs PQ.empty)
  where
    insertPrime :: Int -> [Int] -> IteratorTable -> IteratorTable
    insertPrime p xs = insertComposite (p*p) (map (*p) xs)
    sieve' [] _ = []
    sieve' (x:xs) table
      | nextComposite <= x = sieve' xs (adjust table)
      | otherwise          = x : sieve' xs (insertPrime x xs table)
        where
          nextComposite = minKey table
          adjust table
              | n <= x    = adjust (deleteMinAndInsert n' ns table)
              | otherwise = table
            where
              (n, n':ns) = findMinKeyValue table

