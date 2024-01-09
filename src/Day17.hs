module Day17 where

import Data.Bifunctor (Bifunctor (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Day15 (RIndex, RTuple (..))
import MyLib
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
  | n >= limit = w
  | otherwise = undefined
  where
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
    x = map (flowSide (IM.empty, IM.empty) IS.empty . IS.singleton) toSide
    flowSide acc visited start
      | IS.null start = acc'
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

day17 :: IO ()
day17 = do
  input' <- IM.unionsWith IS.union . mapMaybe (parseMaybe parseInput) . lines <$> readFile "input/input17.txt"
  input' <- IM.unionsWith IS.union . mapMaybe (parseMaybe parseInput) . lines <$> readFile "input/test17.txt"
  putStrLn $ showWater input' initWater
