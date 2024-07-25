module Day11 where

import Paths_AOC2018
import Data.Array
import Data.Bifunctor (Bifunctor (..))
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Debug.Trace
import qualified Data.Array.IArray as I

input = 9995

type PowerGrid = Array Index Int

type Index = (Int, Int)

initArray :: Int -> PowerGrid
initArray sn =
  array
    b
    [ ((x, y), p)
      | (x, y) <- range b,
        let rackID = x + 10,
        let p = (((((rackID * y) + sn) * rackID) `div` 100) `mod` 10) - 5
    ]
  where
    b = ((1, 1), (300, 300))

accumGrid :: PowerGrid -> PowerGrid
accumGrid g = g'
  where
    b = bounds g
    g' =
      array
        b
        [ ((x, y), p x y)
          | (x, y) <- range b
        ]
    p x y = f g' (x - 1, y) + f g' (x, y - 1) + f g (x, y) - f g' (x - 1, y - 1)
      where
        f a i = fromMaybe 0 (a !? i)

calcPower :: PowerGrid -> Index -> Index -> Int
calcPower g i j@(xb, yb) = f j + f i' - f (xa, yb) - f (xb, ya)
  where
    i'@(xa, ya) = bimap (subtract 1) (subtract 1) i
    f i = fromMaybe 0 (g !? i)

(!?) :: (Ix i, I.IArray a e) => a i e -> i -> Maybe e
a !? i
  | inRange (I.bounds a) i = Just (a I.! i)
  | otherwise = Nothing

day11 :: IO ()
day11 = do
  let g = accumGrid $ initArray input
  print $ maximumBy (compare `on` snd) $ [((x, y), calcPower g (x, y) (x + 2, y + 2)) | x <- [1 .. 298], y <- [1 .. 298]]
  print
    . maximumBy (compare `on` snd)
    $ [ ((x, y, size), calcPower g (x, y) (x', y'))
        | x <- [1 .. 300],
          y <- [1 .. 300],
          size <- [1 .. 300],
          let x' = x + size - 1,
          let y' = y + size - 1,
          x' <= 300,
          y' <= 300
      ]
