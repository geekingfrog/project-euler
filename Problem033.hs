module Problem033 (answer) where

answer :: Int
answer = finalDenum `div` (gcd finalDenum finalNum)
  where
    (nums, denums) = unzip [(i, j) | j <- [2..99], i <- [1..j-1], isCurious i j]
    finalNum = product nums
    finalDenum = product denums

isCurious :: Int -> Int -> Bool
isCurious num denum = ((n1/=0) && (d1/=0)) && ((n2/=0) && (d2/=0)) && (
    (n1==d1 && n2 * denum == d2 * num)
    || (n1==d2 && n2 * denum == d1 * num)
    || (n2==d1 && n1 * denum == d2 * num)
    || (n2==d2 && n1 * denum == d1 * num)
  )
  where
    [n1, n2] = decompose num
    [d1, d2] = decompose denum

-- simple decomposition for i<100
decompose n = [n `div` 10, n `mod` 10]
