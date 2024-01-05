{-# LANGUAGE LambdaCase #-}
module Day13 where

import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import MyLib

type RIndex = (Int, Int)

data Cart = C
  { _index :: RIndex,
    _direction :: Direction,
    _turn :: Int
  }
  deriving (Show, Ord, Eq)

readInput :: UArray RIndex Char -> (Set Cart, UArray RIndex Char)
readInput a = (c, a'')
  where
    b = bounds a
    b' = bimap swap swap b
    a' = ixmap b' swap a
    c =
      foldl'
        ( \acc (i, x) -> case x of
            '^' -> Set.insert (C i North 0) acc
            'v' -> Set.insert (C i South 0) acc
            '>' -> Set.insert (C i East 0) acc
            '<' -> Set.insert (C i West 0) acc
            _ -> acc
        )
        Set.empty
        (assocs a')
    a'' = amap (\case 'v' -> '|'; '^' -> '|'; '>' -> '-'; '<' -> '-'; x -> x) a'

day13 :: IO ()
day13 = do
  (cart, m) <- readInput . drawArray @UArray . lines <$> readFile "input/input13.txt"
  print m
