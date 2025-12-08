module Day5 where

import Control.Parallel.Strategies
import Data.Char
import Paths_AOC2018

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

day5 :: IO (String, String)
day5 = do
  input <- filter isAlpha <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let
    !finalAnsa =
      show
        . length
        $ deletePair input
  let
    !finalAnsb =
      show
        . minimum
        $ parMap rpar (\xs -> length $ deletePair $ filter (`notElem` xs) input) alphaList
  pure (finalAnsa, finalAnsb)
