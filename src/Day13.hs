{-# LANGUAGE LambdaCase #-}

module Day13 where

import Control.Monad (foldM)
import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (foldl', insertBy, deleteBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import MyLib

type Index = (Int, Int)

type M = UArray Index Char

data Cart = C
  { _index :: Index,
    _direction :: Direction,
    _turn :: Turn
  }
  deriving (Show, Ord, Eq)

data Turn = L | F | R deriving (Eq, Ord, Show)

instance Enum Turn where
  toEnum n = case n `mod` 3 of
    0 -> L
    1 -> F
    2 -> R
  fromEnum L = 0
  fromEnum F = 1
  fromEnum R = 2

turn :: Turn -> Direction -> Direction
turn L = pred
turn R = succ
turn F = id

tick :: M -> [Cart] -> ([Index],  [Cart])
tick m c = f ([], []) c
  where
    f acc [] = acc
    f acc (c : xs) = f acc' xs'
      where
        (acc', xs')
          | any ((== i) . _index) (snd acc) || any ((== i) . _index) xs = (bimap (i :) (filter ((/= i) . _index)) acc, filter ((/= i) . _index) xs)
          | otherwise = (second (insertBy compareSwap c') acc, xs)
        c' = moveCart m c
        i = _index c'

run :: M -> [Cart] -> ([Index], [Cart])
run m c
  | length c' == 1 = (i, c')
  | otherwise = first (<> i) $ run m c'
  where
    (i, c') = tick m c

moveCart :: M -> Cart -> Cart
moveCart m (C i d t) = C i' d' t'
  where
    a = m ! i
    t' = if a == '+' then succ t else t
    d' = case a of
      '+' -> turn t d
      '/' -> if odd (fromEnum d) then pred d else succ d
      '\\' -> if even (fromEnum d) then pred d else succ d
      _ -> d
    i' = bimap (+ fst i) (+ snd i) $ toIndex d'

readInput :: UArray Index Char -> ([Cart], M)
readInput a = (c, a')
  where
    b = bounds a
    c =
      foldl'
        ( \acc (i, x) -> case x of
            '^' -> insertBy compareSwap (C i North L) acc
            'v' -> insertBy compareSwap (C i South L) acc
            '>' -> insertBy compareSwap (C i East L) acc
            '<' -> insertBy compareSwap (C i West L) acc
            _ -> acc
        )
        []
        (assocs a)
    a' = amap (\case 'v' -> '|'; '^' -> '|'; '>' -> '-'; '<' -> '-'; x -> x) a

compareSwap = compare `on` swap . _index

day13 :: IO ()
day13 = do
  (cart, m) <- readInput . drawArray @UArray . lines <$> readFile "input/input13.txt"
  -- (cart, m) <- readInput . drawArray @UArray . lines <$> readFile "input/test13.txt"
  let ans = run m cart
  print $ last $ fst ans
  print $ _index $ head $ snd ans
  print ans
