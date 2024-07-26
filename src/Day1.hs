module Day1 where

import Data.Maybe (mapMaybe)
import MyLib (firstRepeat', signedInteger)
import Paths_AOC2018
import Text.Megaparsec (parseMaybe)

day1 :: IO ()
day1 = do
  input <- mapMaybe (parseMaybe signedInteger) . lines <$> (getDataDir >>= readFile . (++ "/input/input1.txt"))
  putStrLn
    . ("day1a: " ++)
    . show
    $ sum input
  putStrLn
    . ("day1b: " ++)
    . show
    . fmap snd
    . firstRepeat'
    . scanl (+) 0
    $ cycle input
