module Day22 where

import Control.Monad (unless)
import Control.Monad.ST.Strict (ST, runST)
import Data.Array (Array)
import Data.Array.IArray (amap)
import Data.Array.IArray qualified as I
import Data.Array.MArray qualified as MA
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Map qualified as Map
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as P
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Mutable (STVector)
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import MyLib (drawGraph)
import Paths_AOC2018
import Data.List (stripPrefix)
import Data.List.Split (splitOn)

timesX = 16807

timesY = 48271

modulo = 20183

type Index = (Int, Int)

data Tool = Neither | Torch | Gear deriving (Show, Eq, Ord)

type M = UArray Index Word8

type Q = MinPQueue Int (Int, GameState)

type GameState = (Index, Tool)

instance Enum Tool where
  fromEnum Neither = 0
  fromEnum Torch = 1
  fromEnum Gear = 2
  toEnum n = case n `mod` 3 of
    0 -> Neither
    1 -> Torch
    2 -> Gear

size = 8
astarST :: M -> Index -> STUArray s Index Word8 -> STVector s [(Int, GameState)] -> Int -> ST s Int
astarST m fin visited q bucket = do
  s <- MV.unsafeExchange q bucket []
  if null s then astarST m fin visited q (succ bucket `mod` size) else f visited q s
  where
    bm = I.bounds m
    f visited q [] = astarST m fin visited q bucket
    f visited q ((r, (i, t)) : xs) | i == fin && t == Torch = pure r
    f visited q ((r, (i, t)) : xs) = do
      b <- hasVisited visited i t
      if b
        then f visited q xs
        else do
          MA.modifyArray' visited i (`setBit` fromEnum t)
          let n = next' bm m fin r i t
          mapM_ (g visited q) n
          f visited q xs
    g visited q (hue, s@(r, (i, t))) = do
      b <- hasVisited visited i t
      unless b $ MV.modify q (s :) (hue `mod` size)
    hasVisited visited i t = (`testBit` fromEnum t) <$> MA.readArray visited i

runAStar :: M -> Index -> Int
runAStar m fin = runST $ do
  visited <- MA.newArray (I.bounds m) 0
  q <- MV.replicate size []
  MV.write q (hue fin 0 (0, 0) `mod` size) [(0, ((0, 0), Torch))]
  astarST m fin visited q 0

astar :: M -> Index -> Set GameState -> Q -> Maybe Int
astar m fin visited Empty = Nothing
astar m fin visited ((_, (r, (i, t))) :< _) | i == fin && t == Torch = Just r
astar m fin visited ((_, (_, start)) :< q') | start `Set.member` visited = astar m fin visited q'
astar m fin visited ((_, (r, start@(i, t))) :< q') = astar m fin visited' (P.union q' start')
  where
    b = I.bounds m
    visited' = Set.insert start visited
    start' = P.fromList $ next b m fin visited' r i t

next' b m fin r i t =
  [ (hue fin (r + 7) i, (r + 7, (i, t')))
  | t' <- [succ t, pred t]
  , fromEnum t' /= fromIntegral (m I.! i)
  ]
    ++ [ (hue fin (r + 1) i', (r + 1, (i', t)))
       | i' <- map (bimap (+ fst i) (+ snd i)) adjacent
       , b `I.inRange` i'
       , fromEnum t /= fromIntegral (m I.! i')
       ]
next b m fin visited r i t =
  [ (hue fin (r + 7) i, (r + 7, (i, t')))
  | t' <- [succ t, pred t]
  , fromEnum t' /= fromIntegral (m I.! i)
  , (i, t') `Set.notMember` visited
  ]
    ++ [ (hue fin (r + 1) i', (r + 1, (i', t)))
       | i' <- map (bimap (+ fst i) (+ snd i)) adjacent
       , b `I.inRange` i'
       , fromEnum t /= fromIntegral (m I.! i')
       , (i', t) `Set.notMember` visited
       ]
hue (fx, fy) r (x, y) = r + abs (fx - x) + abs (fy - y)

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

x ** y = (x * y) `mod` modulo

x +* y = (x + y) `mod` modulo


day22 :: IO (String, String)
day22 = do
  [a, b'] <- lines <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  let
    Just depth = read @Int <$> stripPrefix "depth: " a
    Just target = (\[a, b] -> (read @Int a, read @Int b)) . splitOn "," <$> stripPrefix "target: " b'
    b0 = ((0, 0), target)
    b = ((0, 0), (fst target * 7, snd target * 7))
    geologicErosionIndex =
      I.array @Array
        b
        [ (i, (g, e))
        | i@(x, y) <- I.range b
        , let g = f x y
        , let e = g +* depth
        ]
      where
        f 0 y = y * timesY
        f x 0 = x * timesX
        f x y | (x, y) == target = 0
        f x y = snd (geologicErosionIndex I.! (x - 1, y)) * snd (geologicErosionIndex I.! (x, y - 1))
    riskMap :: M
    riskMap = I.array b (map (second (fromIntegral . (`mod` 3) . snd)) a)
      where
        b = I.bounds geologicErosionIndex
        a = I.assocs geologicErosionIndex
    !finalAnsa =
      show
        . sum
        . map (fromIntegral :: Word8 -> Int)
        . I.elems
        $ I.ixmap b0 id riskMap
  let
    !finalAnsb =
      show $ runAStar riskMap target
  -- show $ astar riskMap target Set.empty (P.singleton (hue target 0 (0, 0)) (0, ((0, 0), Torch)))
  pure (finalAnsa, finalAnsb)
