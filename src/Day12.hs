module Day12 where

import Control.Applicative.Combinators (count)
import Data.Array
import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.List (dropWhileEnd, findIndex, groupBy)
import Data.List.Split (divvy, splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Debug.Trace (traceShow)
import Paths_AOC2018

type Rule = Map [Bool] Bool

s = [False, False, False]

next rule (x, ls) = (x - 1 + length a, dropWhileEnd not b)
  where
    ls' = map (rule Map.!) (divvy 5 1 (s <> ls <> s))
    (a, b) = span not ls'

findCycle l = go l l 0 1 2
  where
    go (x : xs) (_ : y : ys) tu ra li
      | x == y = Just (ra - tu, tu, x)
      | tu + ra >= li = go (y : ys) (y : ys) ra (ra + 1) (li * 2)
      | otherwise = go (x : xs) (y : ys) tu (ra + 1) li
    go _ _ _ _ _ = Nothing

calc (x, ls) = sum $ map fst $ filter snd $ zip [x ..] ls

n = 50000000000

day12 :: IO (String, String)
day12 = do
  g : r : _ <- splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input12.txt"))
  let initState = map (== '#') $ drop (length "initial state: ") g
      rules = Map.fromList $ map ((\[x, y] -> (x, head y)) . map (map (== '#')) . splitOn " => ") $ lines r
      xs = iterate (next rules) (0, initState)
      diff = divvy 3 1 xs
      Just stable = findIndex (\[a, b, c] -> calc c - calc b == calc b - calc a) diff
  let
    !finalAnsa =
      show
        . calc
        $ xs !! 20
  let
    !finalAnsb =
      show $
        calc (xs !! stable) + (n - stable) * (calc (xs !! (stable + 1)) - calc (xs !! stable))
  pure (finalAnsa, finalAnsb)
