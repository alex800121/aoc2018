{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

module Day15 where

import Control.Applicative (Alternative (..))
import Control.Monad (foldM_)
import Control.Monad.ST.Strict (ST, runST)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Char (intToDigit)
import Data.DoubleWord (BinaryWord (leadingZeroes, trailingZeroes))
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.Function (fix, on)
import Data.List (foldl', sort, unfoldr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as MSV
import Data.Vector.Strict qualified as V
import Data.Word (Word32)
import GHC.Generics (Generic)
import MyLib (drawMap)
import Numeric (showIntAtBase)
import Optics
import Paths_AOC2018

type Side = Bool

pattern Goblin = False

pattern Elf = True

adjacent = [P 0 (-1), P (-1) 0, P 1 0, P 0 1]

P a b +& P c d = P (a + c) (b + d)

data Unit = Unit
  { hp :: Int
  , ap :: Int
  , side :: Side
  }
  deriving (Show, Eq, Generic, Ord)

data Point = P {x :: Int, y :: Int} deriving (Show, Eq, Generic)

instance Ord Point where
  compare a b = on compare y a b <> on compare x a b

readInput = second (second (SV.fromList @Word32 . ($ []))) . foldl' f (Map.empty, ((0, 0), id)) . zip [0 ..]
  where
    f (m, ((e, g), v)) (i, xs) = second (second (\x -> v . (x :))) $ foldl' (f1 i) (m, ((e, g), 0)) (zip [0 ..] xs)
    f1 i (m, ((e, g), n)) (j, x) = case x of
      '#' -> (m, ((e, g), n `setBit` j))
      'E' -> (Map.insert (P j i) (Unit 200 3 Elf) m, ((e + 1, g), n))
      'G' -> (Map.insert (P j i) (Unit 200 3 Goblin) m, ((e, g + 1), n))
      _ -> (m, ((e, g), n))

intersect a b = f 0
  where
    f x =
      liftA2 (,) <$> MSV.readMaybe a x <*> MSV.readMaybe b x >>= \case
        Nothing -> pure Nothing
        Just (n0, n1) | n <- n0 .&. n1, n /= 0 -> pure $ Just (trailingZeroes n, x)
        _ -> f (x + 1)

expand wall v = snd <$> MSV.ifoldM' f (0, False) v
  where
    f (prev, acc) i curr = do
      next <- fromMaybe 0 <$> MSV.readMaybe v (i + 1)
      w <- MSV.read wall i
      let curr' = (prev .|. (curr `shiftR` 1) .|. curr .|. (curr `shiftL` 1) .|. next) .&. complement w
          acc' = acc || (curr /= curr')
      MSV.write v i curr'
      pure (curr, acc')

play b w0 r0 i0 v0 (m0, e0, g0)
  | e0 == 0 || g0 == 0 = Just $ r0 * sum (Map.map hp mFiltered)
  | i0 >= length v0 = play b w0 (r0 + 1) 0 vFiltered (mFiltered, e0, g0)
  | attHp <= 0 = play b w0 r0 (i0 + 1) v0 (m0, e0, g0)
  | Just (m1, e1, g1) <- attack attP0 attAp attSide m0 e0 g0 =
      if b && e1 /= e0 then Nothing else play b w0 r0 (i0 + 1) v0 (m1, e1, g1)
  | (m1, e1, g1) <- fromMaybe (mMoved, e0, g0) (attack attP1 attAp attSide mMoved e0 g0) =
      if b && e1 /= e0 then Nothing else play b w0 r0 (i0 + 1) vMoved (m1, e1, g1)
  where
    mFiltered = Map.filter ((> 0) . hp) m0
    vFiltered = V.fromList $ Map.keys mFiltered
    attP0 = v0 V.! i0
    u0@(Unit attHp attAp attSide) = m0 Map.! attP0
    attP1 = fromMaybe attP0 $ bfs w0 m0 attP0 attSide
    mMoved = Map.insert attP1 u0 $ Map.delete attP0 m0
    vMoved = v0 V.// [(i0, attP1)]

fromInt i = P (i `shiftR` 8) (i `mod` bit 8)

toInt (P x y) = (x `shiftL` 8) + y

overlap d p = MSV.ifoldM' (f p) Nothing d
  where
    f p acc i x = do
      y <- MSV.read p i
      let n = trailingZeroes (x .&. y)
      if n < finiteBitSize x
        then pure (acc <|> Just (P n i))
        else pure acc

showM = unlines . map (reverse . showBin) . SV.toList @Word32

bfs w0 m0 p0 s0 = runST $ do
  w <- SV.thaw w0
  p <- MSV.replicate (SV.length w0) (0 :: Word32)
  MSV.modify p (`setBit` x p0) (y p0)
  d <- MSV.replicate (SV.length w0) (0 :: Word32)
  traverse_ (f w d) $ Map.toList m0
  expand w d
  t <- g w d p
  case t of
    Nothing -> pure Nothing
    Just (P n i) -> do
      p <- MSV.replicate (SV.length w0) (0 :: Word32)
      MSV.modify p (`setBit` n) i
      d <- MSV.replicate (SV.length w0) (0 :: Word32)
      MSV.modify d (`setBit` x p0) (y p0)
      expand w d
      g w d p
  where
    g w d p = do
      x <- overlap d p
      if isNothing x
        then do
          b <- expand w p
          if b then g w d p else pure Nothing
        else pure x
    f :: MSV.STVector s Word32 -> MSV.STVector s Word32 -> (Point, Unit) -> ST s ()
    f w d (P x y, u)
      | hp u > 0 && side u == s0 = MSV.modify w (`setBit` x) y
      | hp u > 0 = MSV.modify d (`setBit` x) y
      | otherwise = pure ()

attack attP0 attAp attSide m0 e0 g0
  | (defHp0, defP0, d0) : _ <-
      sort
        [ (hp d, p, d)
        | i <- adjacent
        , let p = attP0 +& i
        , p `Map.member` m0
        , let d = m0 Map.! p
        , side d /= attSide
        , hp d > 0
        ]
  , defHp1 <- defHp0 - attAp
  , m1 <- Map.adjust (set #hp defHp1) defP0 m0
  , (e1, g1) <-
      if
        | defHp1 > 0 -> (e0, g0)
        | attSide -> (e0, g0 - 1)
        | otherwise -> (e0 - 1, g0) =
      Just (m1, e1, g1)
  | otherwise = Nothing

showBin x = showIntAtBase 2 (\case 0 -> '.'; 1 -> '#') x ""

showState w m = unlines $ Map.foldlWithKey' f (map (reverse . showBin) (SV.toList w)) m
  where
    f v (P x y) u = over (ix y) (<> ('(' : show (hp u) <> ")")) $ set (ix y % ix x) n v
      where
        n = case side u of
          Elf -> 'E'
          Goblin -> 'G'

binSearch f m = go0 1 m
  where
    go0 n m
      | f (n + m) = go1 m (n + m)
      | otherwise = go0 (n * 2) m
    go1 n m
      | n == c = (n, m)
      | f c = go1 n c
      | otherwise = go1 c m
      where
        c = (n + m) `div` 2

addAp x = Map.map (\y -> if side y then set #ap x y else y)

day15 :: IO (String, String)
day15 = do
  (m0, ((e0, g0), w0)) <- readInput . lines <$> (getDataDir >>= readFile . (++ "/input/input15.txt"))
  let v0 = V.fromList $ Map.keys m0
      (n, m) = binSearch (\x -> isJust (play True w0 0 0 v0 (addAp x m0, e0, g0))) 4
  let
    !finalAnsa =
      show $
        play False w0 0 0 v0 (m0, e0, g0)
  let
    !finalAnsb =
      show $
        play True w0 0 0 v0 (addAp m m0, e0, g0)
  pure (finalAnsa, finalAnsb)
