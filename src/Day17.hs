module Day17 where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Either (partitionEithers)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace
import MyLib
import Paths_AOC2018
import Text.Megaparsec
import Text.Megaparsec.Char

data WaterType = Down | Side | Stasis deriving (Show, Eq, Enum, Ord, Bounded)

type M = IntMap IntSet

type Water = IntMap (IntMap WaterType)

showWater :: M -> Water -> String
showWater m w = unlines $ drawGraph (fromMaybe ' ') $ Map.union m' w'
  where
    m' = Map.fromList [((x, y), '#') | (y, xs) <- IM.assocs $ IM.map IS.toList m, x <- xs]
    w' = Map.fromList [((x, y), f i) | (y, xs) <- IM.assocs $ IM.map IM.assocs w, (x, i) <- xs]
    f Down = '|'
    f Side = '~'
    f Stasis = '.'

fillByLayer :: Int -> M -> Int -> Water -> Water
fillByLayer limit m n w
  -- \| trace (showWater m w) False = undefined
  | n >= limit = w
  | IM.null stasis = fillByLayer limit m (n + 1) w'
  | otherwise = fillByLayer limit m (n - 1) w'
  where
    w' = IM.insert n currentLayerW' $ IM.insert (n + 1) nextLayerW' w
    currentLayerW = fromMaybe IM.empty $ w IM.!? n
    currentLayerM = fromMaybe IS.empty $ m IM.!? n
    nextLayerW = fromMaybe IM.empty $ w IM.!? (n + 1)
    nextLayerM = fromMaybe IS.empty $ m IM.!? (n + 1)
    blocked k = k `IS.member` nextLayerM || nextLayerW IM.!? k == Just Stasis
    (flowDown, toSide) =
      IM.foldlWithKey'
        ( \acc k a ->
            let w' = nextLayerW IM.!? k
             in case a of
                  Down | blocked k -> second (k :) acc
                  Down | fromMaybe Down (nextLayerW IM.!? k) == Down -> first (IM.insert k Down) acc
                  _ -> acc
        )
        (IM.empty, [])
        currentLayerW
    (stasis, (down, side)) =
      bimap IM.unions (bimap IM.unions IM.unions . unzip)
        . partitionEithers
        $ map (flowSide (IM.empty, IM.empty) IS.empty . IS.singleton) toSide
    down' = IM.union down flowDown
    currentLayerW' = IM.union (IM.unions [flowDown, stasis, down', side]) currentLayerW
    nextLayerW' = IM.union down' nextLayerW
    flowSide acc visited start
      | IS.null start = if IM.null (fst acc') then Left $ IM.map (const Stasis) $ snd acc' else Right acc'
      | otherwise = flowSide acc' visited' start'
      where
        visited' = IS.union visited start
        (s0, s1) = IS.partition blocked start
        acc' = bimap (IM.union (IM.fromSet (const Down) s1)) (IM.union (IM.fromSet (const Side) s0)) acc
        start' =
          IS.filter (\k -> k `IS.notMember` visited' && k `IS.notMember` currentLayerM && currentLayerW IM.!? k /= Just Stasis)
            . IS.unions
            $ map (`IS.map` s0) [succ, pred]

parseInput :: Parser M
parseInput = do
  a <- (char 'x' >> pure second) <|> (char 'y' >> pure first)
  char '='
  iA <- signedInteger
  char ',' >> space
  b <- (char 'x' >> pure second) <|> (char 'y' >> pure first)
  char '='
  iB <- signedInteger `sepBy` string ".."
  pure $
    IM.unionsWith IS.union $
      [ uncurry
          IM.singleton
          (second IS.singleton $ a (const iA) $ b (const iBs) (0, 0))
      | iBs <- [minimum iB .. maximum iB]
      ]

initWater :: Water
initWater = IM.singleton 0 (IM.singleton 500 Down)

day17 :: IO (String, String)
day17 = do
  input' <- IM.unionsWith IS.union . mapMaybe (parseMaybe parseInput) . lines <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  -- input' <- IM.unionsWith IS.union . mapMaybe (parseMaybe parseInput) . lines <$> readFile "input/test17.txt"
  let (minY, maxY) = (,) <$> minimum <*> maximum $ IM.keys input'
      ans = fillByLayer maxY input' 0 initWater
      m = showWater input' ans
  -- writeFile "output" m
  -- print $ length $ filter (`elem` "|.~") m
  let
    !finalAnsa =
      show
        . sum
        . IM.map length
        $ IM.filterWithKey (\k _ -> k >= minY && k <= maxY) ans
  let
    !finalAnsb =
      show
        . sum
        . IM.map (length . IM.filter (== Stasis))
        $ IM.filterWithKey (\k a -> k >= minY && k <= maxY) ans
  pure (finalAnsa, finalAnsb)
