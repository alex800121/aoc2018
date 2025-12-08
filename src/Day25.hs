module Day25 where

import Data.List (partition)
import Data.List.Split (splitOn)
import Paths_AOC2018

manhattan :: [Int] -> [Int] -> Int
manhattan a = sum . map abs . zipWith subtract a

constellations :: [[Int]] -> [[[Int]]]
constellations [] = []
constellations (x : xs) = go [] [] [x] xs
  where
    go accAll accCurrent active [] = (active <> accCurrent) : accAll
    go accAll accCurrent [] (x : xs) = go (accCurrent : accAll) [] [x] xs
    go accAll accCurrent active xs = go accAll (active <> accCurrent) a b
      where
        (a, b) = partition (\x -> any ((<= 3) . manhattan x) active) xs

day25 :: IO (String, String)
day25 = do
  input <- map (map (read @Int) . splitOn ",") . lines <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  -- input <- map (map (read @Int) . splitOn ",") . lines <$> readFile "input/test25.txt"
  let
    !finalAnsa =
      show
        . length
        $ constellations input
  let
    !finalAnsb =
      "Merry Christmas!!!"
  pure (finalAnsa, finalAnsb)
