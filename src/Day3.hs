module Day3 where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib
import Paths_AOC2018
import Text.Megaparsec
import Text.Megaparsec.Char

type Fabric = Vec S2 Range

inputParser :: Parser (Int, Fabric)
inputParser = do
  -- #473 @ 170,393: 29x29
  char '#'
  n <- signedInteger
  string " @ "
  a <- signedInteger
  char ','
  b <- signedInteger
  string ": "
  c <- signedInteger
  char 'x'
  d <- signedInteger
  return (n, Cons (a, a + c) (Cons (b, b + d) Nil))

type Range = (Int, Int)

type S2 = S (S Z)

calcOverlap :: [(Int, Fabric)] -> (Int, [Fabric])
calcOverlap = f Nothing [] []
  where
    f !n rest !acc [] = (fromJust n, acc)
    f !n rest !acc (x : xs) = f n' (x : rest) (xs' <> acc) xs
      where
        n' = n `max` if null xs' && null rest' then Just (fst x) else Nothing
        xs' = mapMaybe (overlapEucVec (snd x) . snd) xs
        rest' = mapMaybe (overlapEucVec (snd x) . snd) rest

calcArea' :: Fabric -> IntSet
calcArea' (Cons (a, b) (Cons (c, d) Nil)) = IS.fromList [x + 1000 * y | x <- [a .. b - 1], y <- [c .. d - 1]]

day3 :: IO (String, String)
day3 = do
  input <- map (fromJust . parseMaybe inputParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input3.txt"))
  let (a, b) = calcOverlap input
  let
    !finalAnsa =
      show
        . IS.size
        . IS.unions
        $ map calcArea' b
  let
    !finalAnsb =
      show $
        a
  pure (finalAnsa, finalAnsb)
