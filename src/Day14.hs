{-# LANGUAGE BangPatterns #-}

module Day14 where

import Control.Monad (unless)
import Control.Monad qualified as MV
import Control.Monad.ST.Strict (ST, runST)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Function (fix)
import Data.List (elemIndex, findIndex, isPrefixOf, scanl', tails)
import Data.STRef.Strict
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word
import Debug.Trace (traceShow)
import Paths_AOC2018


size = 25000000

day14 :: IO (String, String)
day14 = do
  test <- filter isDigit <$> (getDataDir >>= (readFile . (++ "/input/input14.txt")))
  let testV = V.fromList $ map digitToInt test
      len = V.length testV
      (initV, sumV) = runST $ do
        v <- MV.replicate size (1 :: Word8)
        v' <- MV.replicate size (0 :: Word32)
        MV.write v 0 3
        MV.write v 1 7
        go 0 1 2 v
        MV.ifoldM' (\acc i n -> let acc' = (acc * 10 + fromIntegral n) `mod` 1000000 in MV.write v' i acc' >> pure acc') 0 v
        (,) <$> V.freeze v <*> V.freeze v'
        where
          go :: Int -> Int -> Int -> MV.STVector s Word8 -> ST s ()
          go a b l v = do
            iA <- MV.read v a
            iB <- MV.read v b
            let (!x, !y) = let n = iA + iB in if n >= 10 then (1, n - 10) else (0, n)
                !l' = l + 1 + fromIntegral x
                !a' = (a + fromIntegral iA + 1) `mod` l'
                !b' = (b + fromIntegral iB + 1) `mod` l'
            unless (l' > size) (MV.write v (l' - 1) y >> go a' b' l' v)
  let
    !finalAnsa =
      map (intToDigit . fromIntegral)
        . V.toList
        $ V.slice (read @Int test) 10 initV
  let
    !finalAnsb =
      show
        . fmap (subtract (len - 1))
        $ V.elemIndex (read @Word32 test) sumV
  pure (finalAnsa, finalAnsb)
