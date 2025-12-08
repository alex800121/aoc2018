{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

module Day24 where

import Control.Applicative (Alternative (..), optional)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (elemIndex, elemIndices, findIndices, foldl', maximumBy, sortBy)
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Max (MaxPQueue (..))
import Data.PQueue.Prio.Max qualified as MPQ
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Tuple (swap)
import Data.Word (Word32, Word8)
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import MyLib (Parser, signedInteger)
import Optics
import Paths_AOC2018
import Text.Megaparsec (choice, parseMaybe, parseTest, sepBy1)
import Text.Megaparsec.Char (char, space, string)

type Side = Bool

pattern Immune = True

pattern Infection = False

attackTypes = ["slashing", "bludgeoning", "cold", "fire", "radiation"]

type Attack = (Int, Int, Side)

type AttackOrder = MaxPQueue Int Attack

data Army = Army
  { units :: Int
  , hp :: Int
  , attackType :: Int
  , attackDamage :: Int
  , initiative :: Int
  , weak :: Word8
  , immune :: Word8
  }
  deriving (Show, Eq, Generic)

inputParser :: Parser ([Army], [Army])
inputParser =
  (,)
    <$> (string "Immune System:" *> space *> some armyParser)
    <*> (string "Infection:" *> space *> some armyParser)

armyParser :: Parser Army
armyParser = do
  units <- signedInteger
  hp <- string " units each with " *> signedInteger
  (weak, immune) <- string " hit points " *> (wiParser <|> pure (0, 0))
  attackDamage <- string "with an attack that does " *> signedInteger
  attackType <- space *> (fromMaybe 0 <$> (elemIndex <$> choice (map string attackTypes) <*> pure attackTypes))
  initiative <- string " damage at initiative " *> signedInteger
  space
  pure $ Army units hp attackType attackDamage initiative weak immune

wiParser :: Parser (Word8, Word8)
wiParser = do
  char '('
  x <- (string "weak to " *> sepBy1 (choice (map string attackTypes)) (string ", ")) <|> pure []
  optional (string "; ")
  y <- (string "immune to " *> sepBy1 (choice (map string attackTypes)) (string ", ")) <|> pure []
  optional (string "; ")
  z <- (string "weak to " *> sepBy1 (choice (map string attackTypes)) (string ", ")) <|> pure []
  char ')'
  space
  pure (foldl' setBit 0 [i | s <- x <> z, i <- elemIndices s attackTypes], foldl' setBit 0 [i | s <- y, i <- elemIndices s attackTypes])

effectivePower :: Army -> Int
effectivePower a = units a * attackDamage a

sortSelection = sortBy (\x y -> on compare effectivePower y x <> on compare initiative y x)

calcDamage attacker defender
  | dw `testBit` a = 2 * damage
  | di `testBit` a = 0
  | otherwise = damage
  where
    damage = effectivePower attacker
    a = attackType attacker
    dw = weak defender
    di = immune defender

decide :: Side -> [(Int, Army)] -> (Word32, AttackOrder) -> (Int, Army) -> (Word32, AttackOrder)
decide side idefenders (attacked, attackOrder) (i, attacker)
  | null icandidates = (attacked, attackOrder)
  | otherwise = (attacked `setBit` selected, MPQ.insert (initiative attacker) (i, selected, side) attackOrder)
  where
    damage = calcDamage attacker
    icandidates = filter (\(i, x) -> not (testBit attacked i) && damage x > 0) idefenders
    selected = fst $ maximumBy (\(_, x) (_, y) -> on compare damage x y <> on compare effectivePower x y <> on compare initiative x y) icandidates

selectionPhase (immune, infection) = attackOrder
  where
    iimmune = zip [1 ..] immune
    iinfection = zip [1 ..] infection
    (_, immuneAttackOrder) = foldl' (decide Immune iinfection) (0, MPQ.empty) iimmune
    (_, attackOrder) = foldl' (decide Infection iimmune) (0, immuneAttackOrder) iinfection

attack input (iattack, idefend, side) = second f . go . second f $ input
  where
    f = if side then id else swap
    go (k, (attackers, defenders)) = (k + canKill, (attackers, defenders'))
      where
        attacker = attackers !! (iattack - 1)
        defender = defenders !! (idefend - 1)
        damage = calcDamage attacker defender
        canKill = damage `div` hp defender
        defenders' = over (ix (idefend - 1) % #units) (max 0 . subtract canKill) defenders

play input
  -- \| traceShow (attackOrder, input) False = undefined
  | null infection' = Just (Immune, sum (map units immune'))
  | null immune' = Just (Infection, sum (map units infection'))
  | kill == 0 = Nothing
  | otherwise = play (immune', infection')
  where
    sorted = bimap sortSelection sortSelection input
    attackOrder = selectionPhase sorted
    (kill, (immune, infection)) = foldl' attack (0, sorted) attackOrder
    immune' = filter ((> 0) . units) immune
    infection' = filter ((> 0) . units) infection

boost i = map (over #attackDamage (+ i))

binSearch p = go0 1
  where
    go0 a min
      | p max = go1 min max
      | otherwise = go0 (2 * a) min
      where
        max = a + min
    go1 min max
      | c == min = max
      | p c = go1 min c
      | otherwise = go1 c max
      where
        c = (min + max) `div` 2

day24 :: IO (String, String)
day24 = do
  Just input <- parseMaybe inputParser <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
  -- Just input <- parseMaybe inputParser <$> (getDataDir >>= readFile . (++ "/input/test24.txt"))
  let boosted = map (\n -> play $ first (boost n) input) [0 ..]
  let
    !finalAnsa =
      show
        . fmap snd
        $ play input
  let
    !finalAnsb =
      show
        . fmap snd
        . (boosted !!)
        $ binSearch (\n -> maybe False fst (boosted !! n)) 0
  pure (finalAnsa, finalAnsb)
