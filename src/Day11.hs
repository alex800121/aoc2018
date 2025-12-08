module Day11 where

import Control.Monad.ST.Strict (ST)
import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Paths_AOC2018

type PowerGrid = Vector Int

type Index = (Int, Int)

len = 300
fromIndex (x, y) = x * succ len + y
toIndex = (`divMod` succ len)

initArray :: Int -> PowerGrid
initArray sn =
  V.generate
    (succ len * succ len)
    f
  where
    f i
      | x == 0 || y == 0 = 0
      | otherwise = ((((rackID * y) + sn) * rackID) `div` 100) `mod` 10 - 5
      where
        (x, y) = toIndex i
        rackID = x + 10

accumGridST :: STVector s Int -> ST s ()
accumGridST v =
  mapM_
    ( \i ->
        let (x, y) = toIndex i
         in if x == 0 || y == 0
              then pure ()
              else do
                xs <- mapM (MV.read v . fromIndex) [(x - 1, y), (x, y - 1), (x, y), (x - 1, y - 1)]
                let a : b : c : d : _ = xs
                MV.write v i (a + b + c - d)
    )
    [0 .. succ len * succ len]

accumGrid :: PowerGrid -> PowerGrid
accumGrid = V.modify accumGridST

calcPower :: PowerGrid -> Index -> Index -> Int
calcPower g i j@(xb, yb) = f j + f i' - f (xa, yb) - f (xb, ya)
  where
    i'@(xa, ya) = bimap (subtract 1) (subtract 1) i
    f i = g V.! fromIndex i

day11 :: IO (String, String)
day11 = do
  input <- read <$> (getDataDir >>= readFile . (++ "/input/input11.txt"))
  let g = accumGrid $ initArray input
  let
    !finalAnsa =
      show
        . maximumBy (compare `on` snd)
        $ [((x, y), calcPower g (x, y) (x + 2, y + 2)) | x <- [1 .. len - 2], y <- [1 .. len - 2]]
  let
    !finalAnsb =
      show
        . maximumBy (compare `on` snd)
        $ [ ((x, y, size), calcPower g (x, y) (x', y'))
          | size <- [1 .. len]
          , x <- [1 .. len - size + 1]
          , let x' = x + size - 1
          , y <- [1 .. len - size + 1]
          , let y' = y + size - 1
          ]
  pure (finalAnsa, finalAnsb)
