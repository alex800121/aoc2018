module Day5 where

import Data.Char

deletePair :: String -> String
deletePair = f []
  where
    f xs [] = reverse xs
    f [] (x : xs) = f [x] xs
    f (x : xs) (y : ys)
      | isUpper x && toLower x == y = f xs ys
      | isLower x && toUpper x == y = f xs ys
      | otherwise = f (y : x : xs) ys

alphaList = [[x, toUpper x] | x <- ['a' .. 'z']]

day5 :: IO ()
day5 = do
  input <- filter isAlpha <$> readFile "input/input5.txt"
  print $ length $ deletePair input
  print $ minimum $ map (\xs -> length $ deletePair $ filter (`notElem` xs) input) alphaList
