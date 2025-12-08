module Day18 where

import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Char (intToDigit)
import Data.DoubleWord (Word256)
import Data.Foldable (Foldable (..))
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Vector.Strict (Vector)
import Data.Vector.Strict qualified as V
import Numeric
import Paths_AOC2018

type Field = ([Word256], [Word256], [Word256])

readInput = foldl' f [0 :: Word256, 0 :: Word256, 0 :: Word256] . zip [0 ..]
  where
    f [a, b, c] (i, x) = case x of
      '|' -> [a `setBit` (i * 5), b, c]
      '#' -> [a, b `setBit` (i * 5), c]
      '.' -> [a, b, c `setBit` (i * 5)]

mask1, mask32, add13, add15, mask250 :: Word256
mask1 = foldl' (\acc x -> acc `setBit` (x * 5)) 0 [0 .. 49]
mask32 = foldl' (\acc x -> acc `setBit` (x * 5 + 4)) 0 [0 .. 49]
add13 = foldl' (\acc _ -> acc `shiftL` 5 + 13) 0 [0 .. 49]
add15 = foldl' (\acc _ -> acc `shiftL` 5 + 15) 0 [0 .. 49]
mask250 = bit 250 - 1

bin x = showIntAtBase 2 intToDigit x ""

surround :: [Word256] -> [Word256]
surround v = zipWith3 (\a b c -> a + b + c) v3up v3down v2
  where
    vsr = map (`shiftR` 5) v
    vsl = map (`shiftL` 5) v
    v2 = zipWith (+) vsr vsl
    v3 = zipWith (+) v v2
    v3up = drop 1 v3 <> [0]
    v3down = 0 : v3

step :: Field -> Field
step (tree, lumber, open) = (f treeLeft openToTree, f lumberLeft treeToLumber, f openLeft lumberToOpen)
  where
    f = zipWith (.|.)
    sTree = surround tree
    sLumber = surround lumber
    s3Tree = map ((.&. mask1) . (`shiftR` 4) . (+ add13)) sTree
    s3Lumber = map ((.&. mask1) . (`shiftR` 4) . (+ add13)) sLumber
    s1Tree = map ((.&. mask1) . (`shiftR` 4) . (+ add15)) sTree
    s1Lumber = map ((.&. mask1) . (`shiftR` 4) . (+ add15)) sLumber
    openToTree = zipWith (.&.) open s3Tree
    openLeft = zipWith xor open openToTree
    treeToLumber = zipWith (.&.) tree s3Lumber
    treeLeft = zipWith xor tree treeToLumber
    lumberLeft = zipWith3 (\a b c -> a .&. b .&. c) lumber s1Tree s1Lumber
    lumberToOpen = zipWith xor lumber lumberLeft

resourceValue (tree, lumber, _) = sum (map popCount tree) * sum (map popCount lumber)

detectCycle xs = go 0 1 xs xs
  where
    go a b (x : xs) (_ : y : ys)
      | x == y = Just (a, b - a) -- (first occurence, cycle length)
      | otherwise = go (a + 1) (b + 2) xs ys
    go _ _ _ _ = Nothing

n = 1000000000
day18 :: IO (String, String)
day18 = do
  [tree, lumber, open] <- transpose . map readInput . lines <$> (getDataDir >>= readFile . (++ "/input/input18.txt"))
  let l = iterate step (tree, lumber, open)
      Just (fi, cy) = detectCycle l
      le = (n - fi) `mod` cy
      n' = fi + le
  let
    !finalAnsa =
      show
        . resourceValue
        $ l !! 10
  let
    !finalAnsb =
      show
        . resourceValue
        $ l !! n'
  pure (finalAnsa, finalAnsb)
