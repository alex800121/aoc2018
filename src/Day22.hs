{-# LANGUAGE LambdaCase #-}

module Day22 where

import Paths_AOC2018
import Data.Array
import Data.Array.IArray (amap)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Map as Map
import Data.PQueue.Prio.Min (MinPQueue (..))
import qualified Data.PQueue.Prio.Min as P
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (drawGraph)

timesX = 16807

timesY = 48271

depth = 8112

-- depth = 510

-- target = (10, 10)
target = (13, 743)

modulo = 20183

type Index = (Int, Int)

data Tool = Neither | Torch | Gear deriving (Show, Eq, Ord)

type M = Array Index Tool

type Q = MinPQueue Int GameState

type GameState = (Index, Tool)

instance Enum Tool where
  fromEnum Neither = 0
  fromEnum Torch = 1
  fromEnum Gear = 2
  toEnum n = case n `mod` 3 of
    0 -> Neither
    1 -> Torch
    2 -> Gear

dijkstra :: M -> Index -> Set GameState -> Q -> Maybe (Int, GameState)
dijkstra m fin visited Empty = Nothing
dijkstra m fin visited ((r, (i, t)) :< _) | i == fin && t == Torch = Just (r, (i, Torch))
-- dijkstra m fin visited ((r, (i, t)) :< q) | i == fin = dijkstra m fin (Set.insert (i, t) visited) (P.insert (r + 7) (i, Torch) q)
dijkstra m fin visited ((_, start) :< q') | start `Set.member` visited = dijkstra m fin visited q'
dijkstra m fin visited ((r, start) :< q') = dijkstra m fin visited' (P.union q' start')
  where
    visited' = Set.insert start visited
    start' = P.filter (`Set.notMember` visited) $ next m r start

next :: M -> Int -> GameState -> Q
next m risk (i, t) = P.fromList q
  where
    b = bounds m
    q =
      [ (risk + 7, (i, t'))
        | t' <- map ($ t) [succ, pred],
          t' /= m ! i
      ] ++
      [ (risk + 1, (i', t))
        | i' <- map (bimap (+ fst i) (+ snd i)) adjacent,
          b `inRange` i',
          t /= m ! i'
      ]

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

x ** y = (x * y) `mod` modulo

x +* y = (x + y) `mod` modulo

b0 = ((0, 0), target)

b = ((0, 0), (fst target * 7, snd target * 7))

geologicErosionIndex =
  array
    b
    [ (i, (g, e))
      | i@(x, y) <- range b,
        let g = f x y,
        let e = g +* depth
    ]
  where
    f 0 y = y * timesY
    f x 0 = x * timesX
    f x y | (x, y) == target = 0
    f x y = snd (geologicErosionIndex ! (x - 1, y)) * snd (geologicErosionIndex ! (x, y - 1))

riskMap :: M
riskMap = amap (toEnum @Tool . (`mod` 3) . snd) geologicErosionIndex

printGEIndex :: Array Index Index -> String
printGEIndex =
  unlines
    . drawGraph (\case Just 0 -> '.'; Just 1 -> '='; Just 2 -> '|')
    . Map.fromList
    . map (second ((`mod` 3) . snd))
    . assocs

day22 :: IO ()
day22 = do
  print $ sum $ amap ((`mod` 3) . snd) $ ixmap b0 id geologicErosionIndex
  print $ dijkstra riskMap target Set.empty (P.singleton 0 ((0, 0), Torch))
