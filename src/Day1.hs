module Day1 where

import Data.Foldable (find)
import Data.IntSet qualified as IS
import Data.List (elemIndex, scanl')
import Data.Maybe (mapMaybe)
import MyLib (signedInteger)
import Paths_AOC2018
import Text.Megaparsec

f (x : xs) acc = acc : f xs (acc + x)

firstRepeat' = go 0 IS.empty
  where
    go i acc [] = Nothing
    go i acc (x : xs)
      | x `IS.member` acc = Just (i, x)
      | otherwise = go (succ i) (IS.insert x acc) xs

day1 :: IO (String, String)
day1 = do
  input <- mapMaybe (parseMaybe signedInteger) . lines <$> (getDataDir >>= readFile . (++ "/input/input1.txt"))
  let
    !finalAnsa =
      show $
        sum input
  let
    !finalAnsb =
      show
        . fmap snd
        . firstRepeat'
        . scanl (+) 0
        $ cycle input
  pure (finalAnsa, finalAnsb)
