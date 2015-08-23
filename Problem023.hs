module Problem023 (answer) where

import Primes (properDivisors)

import Control.Monad.State
import qualified Data.Set as S

answer :: Int
answer = sum $ evalState (mapM isAbundantSum [1..28123]) S.empty

isAbundantSum :: Int -> State (S.Set Int) Int
isAbundantSum n = do
  s <- get
  let isA = isAbundant n
  let isSum = S.foldr (\a prev -> prev || S.member (n-a) s) False s
  if isA
    then (put $ S.insert n s) >> return 0
    else return 0
  if isSum then return 0 else return n

isAbundant :: Int -> Bool
isAbundant n = (sum $ properDivisors n) > n
