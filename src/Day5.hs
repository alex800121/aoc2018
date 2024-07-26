module Day5 where

import Data.Char (toUpper)
import Paths_AOC2018

react :: String -> Int
react = go 0 ""
  where
    go _ [] (y : ys) = go 1 [y] ys
    go n xs [] = n
    go n (x : xs) (y : ys) = if toUpper x == toUpper y && x /= y then go (n - 1) xs ys else go (n + 1) (y : x : xs) ys

day5 :: IO ()
day5 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  putStrLn
    . ("day5a: " ++)
    . show
    $ react input
  putStrLn
    . ("day5b: " ++)
    . show
    . minimum
    . map (\c -> react (filter (\x -> x /= c && x /= toUpper c) input))
    $ ['a' .. 'z']
