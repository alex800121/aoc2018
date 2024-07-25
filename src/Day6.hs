module Day6 where

import Paths_AOC2018
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.List (findIndex, intersect, sort, tails)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.MultiSet as MS
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (drawGraph)

type M = Map Index (Set Index)

type Index = (Int, Int)

adjacent = [(0, 1), (0, -1), (-1, 0), (1, 0)]

addIndex :: (Num a) => (a, a) -> (a, a) -> (a, a)
addIndex (a, b) (c, d) = (a + c, b + d)

step :: M -> M
step m = ms
  where
    ms =
      Map.union m
        . Map.unionsWith (<>)
        $ map (\x -> Map.mapKeysMonotonic (addIndex x) m Map.\\ m) adjacent

parseInput = Map.fromList . map ((\[x, y] -> ((x, y), Set.singleton (x, y))) . map (read @Int) . splitOn ", ") . lines

toDir :: Int -> Int -> [Int] -> UArray Int Int
toDir limit midX xs = U.array (0, limit) [(i, length (takeWhile (< i) upperX) + length (takeWhile (< i) lowerX)) | i <- [0 .. limit]]
  where
    -- midX = xs !! (length xs `div` 2)
    upperX = takeWhile (< limit) [sum (map (abs . subtract x) xs) | x <- [midX ..]]
    lowerX = takeWhile (< limit) [sum (map (abs . subtract x) xs) | x <- [midX - 1, midX - 2 ..]]

-- calc :: [Index] -> Int -> Int
calc i limit = sum $ map snd ys
  where
    allX = sort $ map fst i
    allY = sort $ map snd i
    midX = allX !! (length allX `div` 2)
    midY = allY !! (length allY `div` 2)
    xArray = toDir limit midX allX
    yArray = toDir limit midY allY
    upperX = takeWhile ((limit >) . snd) [(x, sum (map (abs . subtract x) allX)) | x <- [midX ..]]
    lowerX = takeWhile ((< limit) . snd) [(x, sum (map (abs . subtract x) allX)) | x <- [midX - 1, midX - 2 ..]]
    xs = reverse lowerX <> upperX
    ys = [(x, yArray U.! (limit - snd x)) | x <- xs]

day6 :: IO ()
day6 = do
  input <- parseInput <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  -- input <- parseInput <$> readFile "input/test6.txt"
  let f = Map.fromList $ zip (Map.keys input) (['a' .. 'z'] ++ ['A' .. 'Z'])
      xs = iterate step input
      g (x : y : xs) = (length (Map.filter ((> 1) . Set.size) y) - length (Map.filter ((> 1) . Set.size) x)) : g (y : xs)
      Just n = findIndex (\xs -> all (== head xs) $ take 10 xs) $ tails $ g xs
      h = MS.toOccurList . MS.fromList . map Set.findMin . filter ((== 1) . length) . Map.elems
      day6a = maximum $ map snd $ h (xs !! (n + 2)) `intersect` h (xs !! (n + 3))
      i = Map.keys input
  print day6a
  print $ calc i 10000
