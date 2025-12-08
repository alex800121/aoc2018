module Day9 where

import Control.Monad (foldM_)
import Control.Monad.ST.Strict (ST, runST)
import Data.Foldable (foldlM)
import Data.Maybe (mapMaybe)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace (traceM)
import Paths_AOC2018 (getDataDir)
import Text.Read (readMaybe)

play players lastMarble = runST $ do
  v <- MV.new needed
  score <- MV.replicate players (0 :: Int)
  MV.write v 0 (0 :: Int)
  foldM_ (go v score) (0, 0) [0, 23 .. lastMarble - 23]
  -- V.freeze score
  MV.foldl' max 0 score
  where
    rounds = lastMarble `div` 23
    needed = 37 * rounds + 2
    go :: STVector s Int -> STVector s Int -> (Int, Int) -> Int -> ST s (Int, Int)
    go v score (h, t) x = do
      mapM_ (f v) [1 .. 18]
      n <- MV.read v (t + 18)
      MV.modify score (+ (n + x + 23)) ((x + 23) `mod` players)
      MV.write v (h + 37) (x + 19)
      mapM_ (g v) [0 .. 2]
      pure (h + 37, t + 16)
      where
        f v y =
          MV.read v (t + y - 1)
            >>= MV.write v (h + y * 2 - 1)
            >> MV.write v (h + 2 * y) (x + y)
        g v n =
          MV.read v (t + 19 + n)
            >>= MV.write v (t + 16 + n * 2)
            >> MV.write v (t + 17 + n * 2) (x + 20 + n)

day9 :: IO (String, String)
day9 = do
  [players, lastMarble] <- mapMaybe (readMaybe @Int) . words <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  let
    !finalAnsa =
      show $
        play players lastMarble
  let
    !finalAnsb =
      show $
        play players (lastMarble * 100)
  pure (finalAnsa, finalAnsb)
