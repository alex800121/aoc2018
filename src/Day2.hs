module Day2 where

import Paths_AOC2018
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.List (subsequences, (\\), find, intersect)
import Debug.Trace

day2b :: [String] -> String
-- day2b (x : xs) | traceShow (x, find (\y -> 1 == length (x \\ y)) xs) False = undefined
day2b (x : xs) = maybe (day2b xs) (`intersect` x) (find (\y -> 1 == length (filter id $ zipWith (/=) x y)) xs)
  
day2 :: IO ()
day2 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  print $ product $ map (\x -> length $ filter (any ((== x) . snd) . MS.toOccurList) $ map MS.fromList input) [2, 3]
  putStrLn $ day2b input
