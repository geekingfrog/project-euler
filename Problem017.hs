{-# LANGUAGE BangPatterns #-}

module Problem017 (answer) where

-- Using a state monad here is completely overengineered
-- but it's cool for learning purpose

import qualified Control.Monad.State as S
import qualified Data.Map as M

answer :: Int
answer = countUpTo 1000

countUpTo :: Int -> Int
countUpTo n = sum $ S.evalState (mapM numberOfLetter [1..n]) M.empty

numberOfLetter :: Int -> S.State (M.Map Int Int) Int
numberOfLetter n = do
  m <- S.get
  case M.lookup n m of
    Just count -> return count
    Nothing -> do
      let count = countLetters n
      S.put (M.insert n count m)
      return count


countLetters :: Int -> Int
countLetters n
  | n == 1000 = length "onethousand"
  | n == 0    = undefined
  | n == 1    = length "one"
  | n == 2    = length "two"
  | n == 3    = length "three"
  | n == 4    = length "four"
  | n == 5    = length "five"
  | n == 6    = length "six"
  | n == 7    = length "seven"
  | n == 8    = length "eight"
  | n == 9    = length "nine"
  | n == 10   = length "ten"
  | n == 11   = length "eleven"
  | n == 12   = length "twelve"
  | n == 13   = length "thirteen"
  | n == 14   = length "fourteen"
  | n == 15   = length "fifteen"
  | n == 16   = length "sixteen"
  | n == 17   = length "seventeen"
  | n == 18   = length "eighteen"
  | n == 19   = length "nineteen"
  | n == 20   = length "twenty"
  | n == 30   = length "thirty"
  | n == 40   = length "forty"
  | n == 50   = length "fifty"
  | n == 60   = length "sixty"
  | n == 70   = length "seventy"
  | n == 80   = length "eighty"
  | n == 90   = length "ninety"
  | n < 100   = countLetters (10*(n `div` 10)) + countLetters (n `mod` 10)
  | n `mod` 100 == 0 = countLetters (n `div` 100) + length "hundred"
  | n < 1000  = countLetters (n `div` 100) + 10 + countLetters (n `mod` 100) -- and + hundred
  | otherwise = undefined
