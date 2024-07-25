{-# LANGUAGE LambdaCase #-}
module Day10 where

import Paths_AOC2018
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib
import Text.Megaparsec (many, optional, parseMaybe, parseTest, some)
import Text.Megaparsec.Char (char, space, string)
import Debug.Trace
import qualified Data.Map.Strict as Map
import Data.Bifunctor (Bifunctor(..))

type Index = (Int, Int)

type Velo = (Int, Int)

type Light = (Index, Velo)

type Graph = Set Light

calcGraph :: Int -> Graph -> (Int, [Graph])
calcGraph r g = (n, map (gs !!) [n - r .. n + r])
  where
    gs = iterate next g
    f n (x : y : xs)
      | calcDist x >= calcDist y = f (n + 1) (y : xs)
      | otherwise = n
    n = f 0 gs

calcDist :: Graph -> Int
calcDist g = (maxX - minX) + (maxY - minY)
  where
    (xs, ys) = unzip $ map fst (Set.toList g)
    f = (,) <$> minimum <*> maximum
    (minX, maxX) = f xs
    (minY, maxY) = f ys

next :: Graph -> Graph
next = Set.map go
  where
    go ((x, y), (vx, vy)) = ((x + vx, y + vy), (vx, vy))

lightParser :: Parser Light
lightParser = do
  string "position=<" >> space
  x <- signedInteger
  char ',' >> space
  y <- signedInteger
  string "> velocity=<" >> space
  vx <- signedInteger
  char ',' >> space
  vy <- signedInteger
  char '>'
  return ((x, y), (vx, vy))

day10 :: IO ()
day10 = do
  input <- Set.fromList . mapMaybe (parseMaybe lightParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input10.txt"))
  let (n, g) = second head $ calcGraph 0 input
  putStrLn . unlines . drawGraph (\case Just _ -> '#'; _ -> ' ') . Map.fromSet (const ()) . Set.map fst $ g 
  print n
