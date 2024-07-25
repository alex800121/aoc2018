{-# LANGUAGE BangPatterns #-}
module Day14 where

import Paths_AOC2018
import Data.Char (digitToInt, intToDigit)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import Data.List (findIndex, tails, isPrefixOf)

test = "556061"
-- test = "59414"

testInt = read @Int test

testIntList = map digitToInt test

genList :: [Int]
genList = 3 : 7 : go 0 1 (S.fromList [3, 7])
  where
    go !a !b s = next <> go a' b' s'
      where
        !iA = s `S.index` a
        !iB = s `S.index` b
        !next = map digitToInt (show (iA + iB))
        s' = s <> S.fromList next
        a' = (a + iA + 1) `mod` length s'
        b' = (b + iB + 1) `mod` length s'

day14 :: IO ()
day14 = do
  putStrLn $ map intToDigit $ take 10 $ drop testInt genList
  print $ findIndex (testIntList `isPrefixOf`) $ tails genList
