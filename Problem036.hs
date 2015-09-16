module Problem036 (answer) where

import Data.List (nub)
import NumUtil (decompose, recompose, toBase)

answer :: Int
answer = sum $ map recompose $ filter (isPalindrome . toBase 2 . recompose) palindroms
  where
    rawpalindroms = concat [[makePalindrome i False, makePalindrome i True] | i <- [1..999]]
    palindroms = nub $ filter ((>0) . length) $ map filterZeros rawpalindroms
    filterZeros [] = []
    filterZeros (0:xs) = init xs
    filterZeros xs = xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

makePalindrome :: Int -> Bool -> [Int]
makePalindrome x shifted = go shifted decomposed decomposed
  where
    decomposed = reverse $ decompose x
    go _ [] acc = acc
    go False (x:xs) acc = go False xs (x:acc)
    go True  (_:xs) acc = go False xs acc
