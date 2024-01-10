module Day18 where

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import MyLib (drawArray, firstCycle')
import Day11 ((!?))

adjacent = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

day18 :: IO ()
day18 = do
  input <- drawArray @UArray . lines <$> readFile "input/input18.txt"
  print $ input !? (0, 1)
  print $ firstCycle' [0,1,2,3,4,1,3,4]
