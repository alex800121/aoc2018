{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.List (foldl')
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import MyLib (Parser, drawGraph, signedInteger)
import Paths_AOC2018
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, space, string)

type Light = ((Int, Int), (Int, Int))

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

move t ((x, y), (vx, vy)) = ((x + vx * t, y + vy * t), (vx, vy))

minMax :: [Light] -> (Int, Int)
minMax = foldl' (\(a, b) ((x, y), _) -> (min a y, max b y)) (maxBound :: Int, minBound)

heu t = uncurry subtract . minMax . map (move t)

binSearch l = go0 1
  where
    go0 t
      | h1 > h0 = go1 0 (t + 1)
      | otherwise = go0 (2 * t)
      where
        h0 = heu t l
        h1 = heu (t + 1) l
    go1 lower upper
      | c == lower = if h0 > h1 then c + 1 else c
      | h1 > h0 = go1 lower c
      | otherwise = go1 c upper
      where
        c = (lower + upper) `div` 2
        h0 = heu c l
        h1 = heu (c + 1) l

draw t =
  unlines
    . drawGraph
      ( \case
          Just _ -> '#'
          _ -> ' '
      )
    . Map.fromList
    . map ((,()) . fst . move t)

day10 :: IO (String, String)
day10 = do
  input <- mapMaybe (parseMaybe lightParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input10.txt"))
  let t = binSearch input
  let
    !finalAnsa =
      ('\n' :) $
        draw t input
  let
    !finalAnsb =
      show $
        t
  pure (finalAnsa, finalAnsb)
