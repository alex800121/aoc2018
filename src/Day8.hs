{-# LANGUAGE DeriveFoldable #-}

module Day8 where

import Data.Maybe (fromJust, mapMaybe)
import MyLib hiding (Tree)
import Text.Megaparsec
import Text.Megaparsec.Char

data Tree' a = Tree
  { _children :: [Tree' a],
    _metadata :: [a]
  }
  deriving (Show, Eq, Ord, Foldable)

type Tree = Tree' Int

parseTree :: Parser Tree
parseTree = do
  a <- signedInteger <* space
  b <- signedInteger <* space
  children <- count a parseTree
  metadata <- count b (signedInteger <* optional space)
  optional space
  return $ Tree children metadata

calcNode :: Tree -> Int
calcNode (Tree c m)
  | null c = sum m
  | otherwise = sum $ mapMaybe (fmap calcNode . (c !?) . subtract 1) m

day8 :: IO ()
day8 = do
  input <- fromJust . parseMaybe parseTree <$> readFile "input/input8.txt"
  -- input <- fromJust . parseMaybe parseTree <$> readFile "input/test8.txt"
  print $ sum input
  print $ calcNode input
