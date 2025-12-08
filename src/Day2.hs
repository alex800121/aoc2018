module Day2 where

import Data.List (find, intersect, subsequences, (\\))
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Debug.Trace
import Paths_AOC2018

day2b :: [String] -> String
-- day2b (x : xs) | traceShow (x, find (\y -> 1 == length (x \\ y)) xs) False = undefined
day2b (x : xs) = maybe (day2b xs) (`intersect` x) (find (\y -> 1 == length (filter id $ zipWith (/=) x y)) xs)

day2 :: IO (String, String)
day2 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  let
    !finalAnsa =
      show
        . product
        $ map (\x -> length $ filter (any ((== x) . snd) . MS.toOccurList) $ map MS.fromList input) [2, 3]
  let
    !finalAnsb =
      day2b input
  pure (finalAnsa, finalAnsb)
