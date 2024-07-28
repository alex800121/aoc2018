{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Day6 where

import Control.Arrow
import Data.Char (chr, ord)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (find, foldl1', groupBy, nub, sort, sortBy, uncons, unfoldr)
import Data.List.Split (splitOn)
import qualified Data.Map.Merge.Strict as Merge
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import MyLib (drawGraph, stablizedBy)
import Paths_AOC2018

type Index = (Int, Int)

type Bounds = (Index, Index)

type M = IntMap Int

calcAdjacent :: Index -> [Index] -> [Index]
calcAdjacent (x, y) = map ((x +) *** (y +))

adjacent = [(0, 1), (1, 0), (0, -1), (-1, 0)]

calcBounds =
  ( ( minimum &&& maximum
    )
      *** ( minimum &&& maximum
          )
  )
    . unzip
    . map fst

manhattan (x, y) = uncurry (+) . (abs . subtract x *** abs . subtract y)

calcDist :: Index -> [(Index, Int)] -> (Int, Int)
calcDist x = go (Right (0, maxBound)) 0
  where
    go (Right (i, d)) s [] = (i, s)
    go (Left _) s [] = (0, s)
    go r@(Right (i, d)) s ((y, n) : xs)
      | d > d' = go (Right (n, d')) s' xs
      | d == d' = go (Left d) s' xs
      | otherwise = go r s' xs
      where
        !s' = s + d'
        !d' = manhattan x y
    go l@(Left d) s ((y, n) : xs)
      | d > d' = go (Right (n, d')) s' xs
      | otherwise = go l s' xs
      where
        !s' = s + d'
        !d' = manhattan x y

calcPoints :: Bounds -> Int -> [(Index, Int)] -> (M, Int)
calcPoints ((minX, maxX), (minY, maxY)) limit ref =
  fst $
    foldl'
      f
      ((IM.empty, 0), Set.empty)
      [ (n, (x, y))
        | x <- [minX .. maxX],
          y <- [minY .. maxY],
          -- let n = calcDist (x, y) ref
          let dist = sort $ map (first (manhattan (x, y))) ref,
          let !sumD = sum $ map fst dist,
          let n = case dist of
                ((x, n) : (y, _) : _) | x /= y -> (n, sumD)
                _ -> (0, sumD)
      ]
  where
    f ((acc, acc'), wrong) ((n, d), (x, y))
      | n `Set.member` wrong = ((acc, acc''), wrong)
      | x `elem` [minX, minY] || y `elem` [maxX, maxY] = ((IM.delete n acc, acc''), Set.insert n wrong)
      | otherwise = ((IM.insertWith (+) n 1 acc, acc''), wrong)
      where
        !acc'' = if d < limit then acc' + 1 else acc'

day6 :: IO ()
day6 = do
  input <-
    (`zip` [1 ..])
      . map (\s -> let [x, y] = splitOn ", " s in (read @Int x, read @Int y))
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  let bounds = calcBounds input
      ans = calcPoints bounds 10000 input
  putStrLn
    . ("day6a: " ++)
    . show
    . maximum
    $ fst ans
  putStrLn
    . ("day6b: " ++)
    . show
    $ snd ans
