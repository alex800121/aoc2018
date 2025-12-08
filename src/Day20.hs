{-# LANGUAGE DeriveFunctor #-}

module Day20 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlpha)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace
import MyLib (Direction (..), Parser, toIndex)
import Paths_AOC2018
import Text.Megaparsec
import Text.Megaparsec.Char

type Index = (Int, Int)

data Unit c = Leaf c | Branch [Tree c]
  deriving (Show, Eq, Ord, Functor)

type Tree c = [Unit c]

tree :: Parser (Tree Char)
tree = many unit

unit :: Parser (Unit Char)
unit = branch <|> leaf

leaf :: Parser (Unit Char)
leaf = Leaf <$> satisfy isAlpha

branch :: Parser (Unit Char)
branch = Branch <$> (char '(' >> (tree `sepBy` char '|') <* char ')')

fromChar :: Char -> Direction
fromChar 'N' = North
fromChar 'S' = South
fromChar 'W' = West
fromChar 'E' = East

regex :: Parser (Tree Char)
regex = char '^' >> tree <* char '$'

buildMap :: Tree Direction -> Map Index (Set Index)
buildMap = f (0, 0) Map.empty
  where
    insert' x y = Map.insertWith Set.union x (Set.singleton y)
    f start acc [] = acc
    f start acc (Leaf d : xs) = f start' acc' xs
      where
        start' = bimap (+ fst start) (+ snd start) $ toIndex d
        acc' = insert' start' start $ insert' start start' acc
    f start acc (Branch br : xs) = f start acc' xs
      where
        acc' = Map.unionWith Set.union acc $ Map.unionsWith Set.union $ map (f start Map.empty) br

bfs :: Index -> Map Index (Set Index) -> Map Index Int
bfs i m = f 0 (Set.singleton i) Map.empty
  where
    f n start acc
      -- \| traceShow n False = undefined
      | Set.null start = acc
      | otherwise = f (n + 1) start' acc'
      where
        acc' = Map.unionWith min acc $ Map.fromSet (const n) start
        start' = Set.filter (`Map.notMember` acc') . Set.unions $ Set.map (m Map.!) start

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

day20 :: IO (String, String)
day20 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  -- input <- init <$> readFile "input/test20.txt"
  let Just t = fmap (fmap fromChar) <$> parseMaybe regex input
      ans = bfs (0, 0) $ buildMap t
  let
    !finalAnsa =
      show $
        maximum ans
  let
    !finalAnsb =
      show
        . length
        $ Map.filter (>= 1000) ans
  pure (finalAnsa, finalAnsb)
