{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day15 where

import Control.Lens (over, set, view)
import Control.Lens.At (Ixed (..))
import Control.Lens.Indexed
import Control.Lens.TH
import Data.Array
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (find, findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import MyLib (drawArray, drawGraph)
import Prelude hiding (round)

data Unit = Unit
  { _moved :: Bool,
    _pos :: RIndex,
    _hp :: Int,
    _ap :: Int,
    _species :: Species
  }
  deriving (Show, Ord, Eq)

data Species = Elf | Goblin deriving (Eq, Ord)

instance Show Species where
  show Elf = "E"
  show Goblin = "G"

instance Enum Species where
  toEnum n = case n `mod` 2 of
    0 -> Elf
    1 -> Goblin
  fromEnum Elf = 0
  fromEnum Goblin = 1

data GameState = G
  { _units :: Set Unit,
    _gameMap :: Array RIndex Space,
    _egNum :: Array Int Int,
    _roundN :: Int
  }
  deriving (Eq, Ord)

showGameState :: GameState -> String
showGameState (G u g _ i) = unlines (drawGraph f g'') ++ u' ++ "\nRound: " ++ show i
  where
    uPos = Map.fromList $ Set.toList $ Set.map ((,) <$> fromIndex . _pos <*> show . _species) u
    g' = Map.fromList . map (bimap fromIndex show) $ assocs g
    g'' = Map.union uPos g'
    f Nothing = ' '
    f (Just x) = head x
    u' = unlines $ map show $ Set.toList u

instance Show GameState where
  show = showGameState

newtype RTuple a b = R {fromIndex :: (a, b)} deriving (Eq, Ix, Functor, Bifunctor)

type RIndex = RTuple Int Int

type Index = (Int, Int)

instance (Show a, Show b) => Show (RTuple a b) where
  show = show . fromIndex

instance (Ord a, Ord b) => Ord (RTuple a b) where
  compare = compare `on` swap . fromIndex

data Space
  = Wall
  | Space
  deriving (Eq, Ord)

instance Show Space where
  show Wall = "#"
  show Space = "."

makeLenses ''Unit
makeLenses ''GameState

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

readInput :: Int -> Int -> Int -> Array Index Char -> GameState
readInput hp gobAp elfAp input = G units s (listArray (0, 1) [elves, goblins]) 0
  where
    unit x i = let ap = if x == Elf then elfAp else gobAp in Unit False i hp ap x
    b = bimap R R (bounds input)
    l = map (first R) $ assocs input
    f = Set.fromList . map (\x -> unit (if snd x == 'E' then Elf else Goblin) (fst x))
    units = f $ filter ((`elem` "EG") . snd) l
    s = array b (map (second (\case '#' -> Wall; _ -> Space)) l)
    elves = length (Set.filter ((== Elf) . _species) units)
    goblins = length (Set.filter ((== Goblin) . _species) units)

round :: GameState -> GameState
round g
  | _moved u = over roundN (+ 1) $ over units (Set.map (set moved False)) g
  | 0 `elem` _egNum g' = (if _moved u' then over roundN (+ 1) else id) g'
  | otherwise = round g'
  where
    us = _units g
    (u, us') = Set.deleteFindMin us
    g' = turn (set units us' g) u
    u' = Set.findMin $ _units g'

turn :: GameState -> Unit -> GameState
turn g u = g'
  where
    u' = move g u
    g' = attack g u'

hasSpace g i = inRange b i && gm ! i /= Wall
  where
    gm = _gameMap g
    b = bounds gm

hasSpecies g sp i = any (\x -> _pos x == i && _species x == sp) $ _units g

hasOpponent g u = hasSpecies g (succ (_species u))

hasTeam g u = hasSpecies g (_species u)

move :: GameState -> Unit -> Unit
move g u
  | any (hasOpponent g u) start = set moved True u
  | otherwise = set moved True $ set pos i' u
  where
    start =
      Map.filterWithKey (\k _ -> hasSpace g k && not (hasTeam g u k)) $
        Map.fromList [(x, x) | let i = _pos u, (a, b) <- adjacent, let x = bimap (+ a) (+ b) i]
    i' = bfs start Set.empty
    bfs next visited
      | Map.null next = view pos u
      | not (Map.null nextToU) = snd i'
      | otherwise = bfs next'' visited'
      where
        (nextToU, next') =
          Map.partitionWithKey
            (\k _ -> hasOpponent g u k)
            next
        i' = Map.findMin nextToU
        visited' = Set.union visited $ Map.keysSet next
        next'' =
          Map.filterWithKey (\k _ -> not (hasTeam g u k) && hasSpace g k && Set.notMember k visited')
            . Map.unionsWith min
            $ map (\(a, b) -> Map.mapKeys (bimap (+ a) (+ b)) next') adjacent

attack :: GameState -> Unit -> GameState
attack g u
  | null ops = over units (Set.insert u) g
  | hp' <= 0 = over (egNum . ix (fromEnum (_species op))) (subtract 1) $ over units (Set.insert u . Set.delete op) g
  | otherwise = over units (Set.insert u . Set.insert op' . Set.delete op) g
  where
    i = map (\(a, b) -> bimap (+ a) (+ b) (_pos u)) adjacent
    ops = Set.filter ((&&) <$> (`elem` i) . _pos <*> (/= _species u) . _species) $ _units g
    op = minimumBy ((compare `on` _hp) <> (compare `on` _pos)) ops
    op' = over hp (subtract (_ap u)) op
    hp' = _hp op'

outcome = (*) <$> _roundN <*> sum . map _hp . Set.toList . _units
day15 :: IO ()
day15 = do
  input <- drawArray @Array . lines <$> readFile "input/input15.txt"
  -- input <- drawArray @Array . lines <$> readFile "input/test15.txt"
  let g = readInput 200 3 3 input
      elves = length $ Set.filter ((== Elf) . _species) $ _units g
      gs = iterate round g
      g' = find (elem 0 . _egNum) gs
      altG =
        find ((== elves) . (! 0) . _egNum . snd) $
        mapMaybe
          ( \x ->
              let y = find (((||) <$> (< elves) . (! 0) <*> (== 0) . (! 1)) . _egNum)
                      . iterate round
                      $ readInput 200 3 x input
               in (x,) <$> y
          )
          [4 ..]
  print $ fmap outcome g'
  print $ fmap (outcome . snd) altG
