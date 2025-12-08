module Day6 where

import Control.Parallel.Strategies
import Data.Array.Unboxed qualified as U
import Data.IntSet qualified as IS
import Data.List (foldl', sort)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Paths_AOC2018

type Index = (Int, Int)

adjacent = [(0, 1), (0, -1), (-1, 0), (1, 0)]

parseInput = map ((\[x, y] -> (x, y)) . map (read @Int) . splitOn ", ") . lines

-- day6b i limit = (minY, maxY)
day6b i limit = sum ys
  where
    allX = sort $ map fst i
    allY = sort $ map snd i
    midX = allX !! (length allX `div` 2)
    midY = allY !! (length allY `div` 2)
    yArray x = length (takeWhile (< x) upperY) + length (takeWhile (< x) lowerY)
    upper all mid = takeWhile (< limit) [sum (map (abs . subtract m) all) | m <- [mid ..]]
    lower all mid = takeWhile (< limit) [sum (map (abs . subtract m) all) | m <- [mid - 1, mid - 2 ..]]
    upperY = upper allY midY
    lowerY = lower allY midY
    upperX = upper allX midX
    lowerX = lower allX midX
    xs = lowerX <> upperX
    ys = map (yArray . (limit -)) xs

day6a l = maximum ans
  where
    lSorted = sort l
    ilSorted = zip [1 ..] lSorted
    maxY = maximum $ map snd l
    minY = minimum $ map snd l
    ans = U.accumArray @U.Array (+) 0 (1, length l) (concatMap (g . reverse) ys)
    g ((i0, (m0, x0)) : a1@(i1, (m1, x1)) : a2@(i2, (m2, x2)) : cs)
      | i1 `IS.member` infinite = g (a1 : a2 : cs)
      | otherwise = (i1, r + l + 1) : g (a1 : a2 : cs)
      where
        l = (x1 - m1 - x0 + m0 - 1) `div` 2
        r = (x2 + m2 - x1 - m1 - 1) `div` 2
    g _ = []
    infinite = IS.fromList $ map fst $ head ys <> last ys <> map head ys <> map last ys
    ys = map (\y -> f y [] ilSorted) [minY .. maxY]
    f row cs [] = cs
    f row [] ((i1, (x, y)) : xs) = f row [(i1, (abs (row - y), x))] xs
    f row cs'@((i0, (m0, x0)) : cs) xs'@((i1, (x1, y1)) : xs)
      | d < -w = f row cs xs'
      | d == -w = f row ((i1, (m1, x1)) : (0, (m0, x0)) : cs) xs
      | d == w = f row ((0, (m1, x1)) : (i0, (m0, x0)) : cs) xs
      | d > w = f row cs' xs
      | otherwise = f row ((i1, (m1, x1)) : (i0, (m0, x0)) : cs) xs
      where
        m1 = abs (row - y1)
        d = m1 - m0
        w = x1 - x0

day6 :: IO (String, String)
day6 = do
  input <- parseInput <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  let
    !finalAnsa =
      show $
        day6a input
  let
    !finalAnsb =
      show $
        day6b input 10000
  pure (finalAnsa, finalAnsb)
