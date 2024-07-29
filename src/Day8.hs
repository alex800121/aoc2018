{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Day8 where

import Control.Arrow
import Control.Monad.Trans.State
import Paths_AOC2018
import Data.Maybe (mapMaybe)
import MyLib ((!?))
import Control.Monad (replicateM)

data Tree a = Tree a [Tree a]
  deriving (Show, Eq, Ord, Functor, Foldable)

type License = Tree [Int]

readLicense :: State [Int] License
readLicense = do
  xs <- get
  let x = head xs
      xs' = tail xs
      y = head xs'
      xs'' = tail xs'
  put xs''
  children <- replicateM x readLicense
  xs <- get
  let (a, b) = splitAt y xs
  put b
  pure $ Tree a children

calcValue :: License -> Int
calcValue (Tree xs []) = sum xs
calcValue (Tree xs ys) = sum $ mapMaybe (fmap calcValue . (ys !?) . subtract 1) xs

day8 :: IO ()
day8 = do
  -- input <- evalState readLicense . map read . words <$> (getDataDir >>= readFile . (++ "/input/test8.txt"))
  input <- evalState readLicense . map read . words <$> (getDataDir >>= readFile . (++ "/input/input8.txt"))
  putStrLn
    . ("day8a: " ++)
    . show
    . sum
    $ sum <$> input
  putStrLn
    . ("day8b: " ++)
    . show
    $ calcValue input
