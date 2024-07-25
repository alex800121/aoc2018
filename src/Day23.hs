module Day23 where

import Paths_AOC2018
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromJust, mapMaybe)
import Data.PQueue.Prio.Max (MaxPQueue (..))
import qualified Data.PQueue.Prio.Max as P
import Debug.Trace
import MyLib (Nat (..), Parser, Vec (..), jointEucVecs, overlapEucVec, signedInteger, subtractEucVecs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (guard)

type Triple a = (a, a, a)

type Index = Triple Int

type Bot = (Index, Int)

type Bot' = Vec S4 Range -- <xyz, -xyz, x-yz, xy-z>

type Range = (Int, Int)

type S4 = S (S (S (S Z)))

convertBot :: Bot -> Bot'
convertBot ((x, y, z), r) = Cons (b, a) $ Cons (d, c) $ Cons (f, e) $ Cons (h, g) Nil
  where
    xyz = x + y + z
    x'yz = -x + y + z
    xy'z = x - y + z
    xyz' = x + y - z
    a = xyz + r + 1
    b = xyz - r
    c = x'yz + r + 1
    d = x'yz - r
    e = xy'z + r + 1
    f = xy'z - r
    g = xyz' + r + 1
    h = xyz' - r

type Q = MaxPQueue Int Bot'

toCenter :: Bot' -> Maybe Index
toCenter (Cons (a, b) (Cons (c, d) (Cons (e, f) (Cons (g, h) Nil))))
  | any (/= 1) [b - a, d - c, f - e, h - g] = Nothing
  | otherwise = Just ((a - c) `div` 2, (a - e) `div` 2, (a - g) `div` 2)

dijkstra :: [Bot'] -> Int -> IntMap Index -> Q -> IntMap Index
dijkstra bots maxN acc q
  | P.null q || i < maxN = acc
  | Just c <- center = dijkstra bots (max i maxN) (IM.insertWith maxBy i c acc) xs
  | otherwise = dijkstra bots maxN acc (P.union xs bot')
  where
    ((i, x), xs) = P.deleteFindMax q
    center = toCenter x
    maxBy a b = if manhattan (0, 0, 0) a > manhattan (0, 0, 0) b then a else b
    bot' = P.fromList $ divideBot bots x

divideBot :: [Bot'] -> Bot' -> [(Int, Bot')]
divideBot bots bot@(Cons (a, b) (Cons (c, d) (Cons (e, f) (Cons (g, h) Nil)))) = do
  x <- [(a, ab), (ab, b)]
  y <- [(c, cd), (cd, d)]
  z <- [(e, ef), (ef, f)]
  w <- [(g, gh), (gh, h)]
  guard $ uncurry (<) x
  guard $ uncurry (<) y
  guard $ uncurry (<) z
  guard $ uncurry (<) w
  let bot' = Cons x (Cons y (Cons z (Cons w Nil)))
      n = length $ mapMaybe (overlapEucVec bot') bots
  pure (n, bot')
  where
    (Cons ab (Cons cd (Cons ef (Cons gh Nil))))  = fmap (\(x, y) -> (x + y) `div` 2) bot

-- overlapBot :: [(Int, Bot')] -> IntMap [Bot']
-- overlapBot = foldr f IM.empty
--   where
--     f (i, x) acc | traceShow (fst <$> IM.maxViewWithKey acc, i) False = undefined
--     f (_, x) acc = IM.filter (not . null) sub
--       where
--         overlapped = IM.map (mapMaybe (overlapEucVec x)) acc
--         add = IM.unionWith (\x y -> (x <> y)) acc $ IM.insert 0 [x] $ IM.mapKeys (+ 1) overlapped
--         sub = IM.unionWith (flip subtractEucVecs) add overlapped

parseBot :: Parser Bot
parseBot = do
  string "pos=<"
  i <- (,,) <$> signedInteger <*> (char ',' >> signedInteger) <*> (char ',' >> signedInteger)
  string ">, r="
  r <- signedInteger
  return (i, r)

manhattan :: (Num a) => (a, a, a) -> (a, a, a) -> a
manhattan (a, b, c) (d, e, f) = abs (a - d) + abs (b - e) + abs (c - f)

inRange' :: Bot -> Bot -> Bool
inRange' a b = manhattan (fst a) (fst b) <= snd a

day23 :: IO ()
day23 = do
  input <- map (fromJust . parseMaybe parseBot) . lines <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  -- input <- map (fromJust . parseMaybe parseBot) . lines <$> readFile "input/test23.txt"
  let maxR = maximum $ map (manhattan (0, 0, 0) . fst) input
      input' = map convertBot input
      searchArea = convertBot ((0, 0, 0), maxR)
  print $ length $ filter (inRange' (maximumBy (compare `on` snd) input)) input
  -- print $ fst $ fromJust $ IM.maxView $ IM.filter (not . null) $ overlapBot $ zip [0..] $ map convertBot input
  print $ manhattan (0, 0, 0) $ maximum $ dijkstra input' 0 IM.empty (P.singleton 1000 searchArea)
