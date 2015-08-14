module Problem005 (answer) where

answer :: Int
answer = floor(2**4) * floor(3**2) * 5 * 7 * 11 * 13 * 17 * 19

-- just see the decomposition in prime factors of [1..20]
-- and then pick the highest power for each factor
