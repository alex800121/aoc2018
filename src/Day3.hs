module Day3 where

import Paths_AOC2018
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib
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
    f n rest acc [] = (fromJust n, acc)
    f n rest acc (x : xs) = f n' (x : rest) (xs' <> acc) xs
      where
        n' = n `max` if null xs' && null rest' then Just (fst x) else Nothing
        xs' = mapMaybe (overlapEucVec (snd x) . snd) xs
        rest' = mapMaybe (overlapEucVec (snd x) . snd) rest

calcArea :: Fabric -> Int
calcArea (Cons (a, b) (Cons (c, d) Nil)) = (b - a) * (d - c)

calcArea' :: Fabric -> Set Range
calcArea' (Cons (a, b) (Cons (c, d) Nil)) = Set.fromList [(x, y) | x <- [a .. b - 1], y <- [c .. d - 1]]

day3 :: IO ()
day3 = do
  input <- map (fromJust . parseMaybe inputParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input3.txt"))
  let (a, b) = calcOverlap input
  print $ length $ Set.unions $ map calcArea' b
  print a
