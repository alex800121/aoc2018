module Day21 where

import Paths_AOC2018
import qualified Data.Vector as V
import Day19 (parseInput, run)
import Data.List (find)
import MyLib (intToBits, firstCycle', firstRepeat')

initVec = V.fromList (replicate 6 0)

day21 :: IO ()
day21 = do
  input <- parseInput <$> (getDataDir >>= readFile . (++ "/input/input21.txt"))
  let xs = map (V.! 5) $ filter ((== 28) . (V.! 1)) $ uncurry run input $ initVec V.// [(0, 0)]
      Just (i, x) = firstRepeat' xs
  print $ head xs
  print $ xs !! (i - 1)
