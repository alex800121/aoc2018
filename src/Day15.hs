{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Control.Lens (over, set, view)
import Control.Lens.TH
import Data.Array
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce
import Data.Foldable (minimumBy)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import MyLib (drawArray)
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
    _gameMap :: Array RIndex Space
  }
  deriving (Eq, Ord)

showGameState :: GameState -> String
showGameState (G u g) = undefined
  where
    uPos = Map.fromList $ Set.toList $ Set.map ((,) <$> _pos <*> show . _species) u

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

readInput :: Int -> Int -> Array Index Char -> GameState
readInput hp ap input = G units s
  where
    unit x i = Unit False i hp ap x
    b = bimap R R (bounds input)
    l = map (first R) $ assocs input
    f = Set.fromList . map (\x -> unit (if snd x == 'E' then Elf else Goblin) (fst x))
    units = f $ filter ((`elem` "EG") . snd) l
    s = array b (map (second (\case '#' -> Wall; _ -> Space)) l)

round :: GameState -> GameState
round g
  | _moved u = over units (Set.map (set moved False)) g
  | otherwise = round (turn (set units us' g) u)
  where
    us = _units g
    (u, us') = Set.deleteFindMin us

turn :: GameState -> Unit -> GameState
turn g u = g'
  where
    u' = move g u
    g' = attack g u'

notWall g i = inRange b i && (gm ! i /= Wall)
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
      Map.filterWithKey (\k _ -> notWall g k) $
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
          Map.filterWithKey (\k _ -> not (hasTeam g u k) && notWall g k && Set.notMember k visited')
            . Map.unionsWith min
            $ map (\(a, b) -> Map.mapKeys (bimap (+ a) (+ b)) next') adjacent

attack :: GameState -> Unit -> GameState
attack g u
  | null ops = over units (Set.insert u) g
  | otherwise = over units (Set.insert u . (if hp' <= 0 then id else Set.insert op') . Set.delete op) g
  where
    i = map (\(a, b) -> bimap (+ a) (+ b) (_pos u)) adjacent
    ops = Set.filter ((`elem` i) . _pos) $ _units g
    op = minimumBy (compare `on` _hp) ops
    op' = over hp (subtract (_ap u)) op
    hp' = _hp op'

day15 :: IO ()
day15 = do
  input <- drawArray @Array . lines <$> readFile "input/input15.txt"
  input <- drawArray @Array . lines <$> readFile "input/test15.txt"
  let g = readInput 200 3 input
  print $ round g
