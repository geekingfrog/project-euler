module Problem009 (answer) where

answer :: Int
answer = head [a*b*c |
                      a <- [1..1000],
                      b <- [1..1000],
                      c <- [1000 - a - b],
                      a*a + b*b == c*c]
