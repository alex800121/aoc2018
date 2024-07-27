module Day5 where

import Data.Char (toUpper)
import Paths_AOC2018

react :: String -> String
react = go ""
  where
    go [] (y : ys) = go [y] ys
    go xs [] = xs
    go (x : xs) (y : ys) = if toUpper x == toUpper y && x /= y then go  xs ys else go  (y : x : xs) ys

day5 :: IO ()
day5 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let input' = react input
  putStrLn
    . ("day5a: " ++)
    $ show $ length input'
  putStrLn
    . ("day5b: " ++)
    . show
    . minimum
    . map (\c -> length $ react (filter (\x -> x /= c && x /= toUpper c) input'))
    $ ['a' .. 'z']
