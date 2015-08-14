module Problem006 (answer) where

answer :: Int
answer = sumSquared - sumOfSquares
  where
    range = [1..100]
    square x = x*x
    sumSquared = square $ sum range
    sumOfSquares = sum (map square range)
