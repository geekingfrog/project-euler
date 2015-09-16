{-# LANGUAGE OverloadedStrings #-}

module Problem022 (answer) where

import Text.ParserCombinators.Parsec
import Data.Char (ord)
import Data.List (sort)

answer :: IO Int
answer = do
  rawNames <- readFile "./data/22.txt"
  let parsed = parse nameFile "namefile" rawNames
  case parsed of
    Left _ -> undefined -- look at that error handling \o/
    Right names -> return . sum $ zipWith (curry prod2) [1..] (map scoreName (sort names))
    -- Right names -> return . sum $ map prod2 (zip [1..] (map scoreName (sort names)))

prod2 :: (Int, Int) -> Int
prod2 (a, b) = a*b

scoreName :: String -> Int
scoreName = sum . map (\x -> ord x - 64)

nameFile :: Parser [String]
nameFile = sepBy name (char ',')

name :: Parser String
name = between (char '"') (char '"') (many (noneOf "\""))
