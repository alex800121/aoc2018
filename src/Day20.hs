{-# LANGUAGE DeriveFunctor #-}
module Day20 where

import Data.Char (isAlpha)
import Text.Megaparsec
import Text.Megaparsec.Char

type Index = (Int, Int)

data Unit c = Leaf [c] | Branch [Tree c]
  deriving (Show, Eq, Ord, Functor)

type Tree c = [Unit c]

day20 :: IO ()
day20 = do
  input <- init <$> readFile "input/input20.txt"
  print input
