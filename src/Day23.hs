module Day23 where

import Data.Function (on)
import Data.List (foldl', maximumBy, nub)
import Data.Maybe (mapMaybe)
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as Q
import MyLib (signedInteger)
import Paths_AOC2018
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, string)

manhattan (a, b, c) (d, e, f) = abs (a - d) + abs (b - e) + abs (c - f)

botParser = do
  string "pos=<"
  x <- signedInteger <* char ','
  y <- signedInteger <* char ','
  z <- signedInteger <* string ">, r="
  r <- signedInteger
  pure ((x, y, z), r)

day23a l = length $ filter (\(a, _) -> manhattan x a <= d) l
  where
    (x, d) = maximumBy (compare `on` snd) l

type Hue = (Int, Int, Int)

split ((x0, y0, z0), (x1, y1, z1)) =
  nub
    [ ((a, b, c), (d, e, f))
    | (a, d) <- [(x0, cx), (cx + 1, x1)]
    , (b, e) <- [(y0, cy), (cy + 1, y1)]
    , (c, f) <- [(z0, cz), (cz + 1, z1)]
    ]
  where
    cx = (x0 + x1) `div` 2
    cy = (y0 + y1) `div` 2
    cz = (z0 + z1) `div` 2

size ((x0, y0, z0), (x1, y1, z1)) = (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)

dist ((x0, y0, z0), (x1, y1, z1)) (a, b, c) = x + y + z
  where
    x = max 0 (x0 - a) + max 0 (a - x1)
    y = max 0 (y0 - b) + max 0 (b - y1)
    z = max 0 (z0 - c) + max 0 (c - z1)

inRange a (b, r) = dist a b <= r

day23b l = go (Q.singleton (hue l x0) (x0, l))
  where
    x0 = foldl' f ((maxBound, maxBound, maxBound), (minBound, minBound, minBound)) l
    f ((a, b, c), (d, e, f)) ((x, y, z), _) = ((min a x, min b y, min c z), (max a x, max b y, max c z))
    go Empty = Nothing
    go (((a, b, c), (x0, l0)) :< q)
      | c == 1 = Just b
      | otherwise = go (q <> Q.fromList [(hue l x, (x, l)) | let l = filter (inRange x0) l0, x <- split x0])

hue l x0 = (negate $ length $ filter (inRange x0) l, dist x0 (0, 0, 0), size x0)

day23 :: IO (String, String)
day23 = do
  input <- mapMaybe (parseMaybe botParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  let
    !finalAnsa =
      show $
        day23a input
  let
    !finalAnsb =
      show $
        day23b input
  pure (finalAnsa, finalAnsb)
