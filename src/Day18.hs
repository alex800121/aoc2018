module Day18 where

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (mapMaybe)
import Day11 ((!?))
import MyLib (drawArray, firstCycle, firstCycle')
import Control.Lens.Tuple (_1, _2)
import Control.Lens (view)

adjacent = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

type Yard = UArray Index Char

type Index = (Int, Int)

step :: Yard -> Yard
step y = y'
  where
    b = U.bounds y
    y' =
      U.array
        b
        [ (i, f i)
          | i <- U.range b
        ]
    f i = case x of
      '.' -> if tree >= 3 then '|' else '.'
      '|' -> if yard >= 3 then '#' else '|'
      '#' -> if yard >= 1 && tree >= 1 then '#' else '.'
      where
        x = y U.! i
        xs = mapMaybe ((y !?) . bimap (+ fst i) (+ snd i)) adjacent
        tree = length $ filter (== '|') xs
        yard = length $ filter (== '#') xs
        space = length $ filter (== '.') xs

day18 :: IO ()
day18 = do
  input <- drawArray @UArray . lines <$> readFile "input/input18.txt"
  -- input <- drawArray @UArray . lines <$> readFile "input/test18.txt"
  let ans = iterate step input
      Just (c, i) = ((,) <$> view _1 <*> view _2) <$> firstCycle' ans
      n = (1000000000 - i) `mod` c + i
      f x = length (filter (== '#') x) * length (filter (== '|') x)
  print $ f $ U.elems $ (!! 10) ans
  print $ f $ U.elems $ ans !! n
