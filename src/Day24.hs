{-# LANGUAGE TemplateHaskell #-}

module Day24 where

import Control.Lens
import Control.Monad (guard, join)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlpha)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (find, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (Parser, firstRepeat', signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (round)

data System = Immune | Infection
  deriving (Show, Eq, Ord)

instance Enum System where
  fromEnum Immune = 0
  fromEnum Infection = 1
  toEnum n = case n `mod` 2 of
    0 -> Immune
    1 -> Infection

data Army = Army
  { _system :: System,
    _unit :: Int,
    _hp :: Int,
    _ap :: (String, Int),
    _weakness :: Set String,
    _immune :: Set String,
    _initiative :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Army

-- 427 units each with 8025 hit points (immune to fire, slashing; weak to cold) with an attack that does 139 bludgeoning damage at initiative 7
parseSpecialty :: Parser (Set String, Set String)
parseSpecialty = do
  char '('
  xs <- (imm <|> wea) `sepBy` string "; "
  string ") "
  return $ foldr (\(a, b) (c, d) -> (a <> c, b <> d)) (Set.empty, Set.empty) xs
  where
    f s g = do
      string s
      xs <- Set.fromList <$> (some (satisfy isAlpha) `sepBy` string ", ")
      return $ g (const xs) (Set.empty, Set.empty)
    imm = f "immune to " first
    wea = f "weak to " second

parseInput :: Parser (IntMap Army)
parseInput = IM.unions <$> ((g <|> h) `sepBy` newline)
  where
    f a b = do
      string a >> newline
      xs <- parseArmy b `sepEndBy` newline
      pure $ IM.fromList [(i, x) | x <- xs, let i = view initiative x]
    g = f "Immune System:" Immune
    h = f "Infection:" Infection

selectionOrder :: IntMap Army -> [Int]
selectionOrder = map fst . sortBy (compareSelection `on` snd) . IM.assocs

selectionPhase :: IntMap Army -> [(Int, Maybe Int)]
selectionPhase army = sortBy (flip compare `on` fst) $ f Set.empty order
  where
    order = selectionOrder army
    f _ [] = []
    f selected (x : xs) = (x, s) : f selected' xs
      where
        selected' = maybe selected (`Set.insert` selected) s
        xArmy = army IM.! x
        s' =
          IM.assocs
            . IM.filterWithKey
              ( \k a ->
                  ((/=) `on` view system) xArmy a
                    && k `Set.notMember` selected
              )
            $ army
        s'' = minimumBy (compareSelectAttack xArmy `on` snd) s'
        s
          | not (null s') && damageDealt xArmy (snd s'') /= 0 = Just $ fst s''
          | otherwise = Nothing

attackPhase :: IntMap Army -> [(Int, Maybe Int)] -> IntMap Army
attackPhase army [] = army
attackPhase army (x : xs)
  | Just y <- y, Just attacker <- attacker = attackPhase (IM.update (attack attacker) y army) xs
  | otherwise = attackPhase army xs
  where
    attacker = army IM.!? fst x
    y = snd x

round :: IntMap Army -> IntMap Army
round army = attackPhase army (selectionPhase army)

attack :: Army -> Army -> Maybe Army
attack attacker defender = if unit' <= 0 then Nothing else Just $ set unit unit' defender
  where
    ap' = damageDealt attacker defender
    killed = ap' `div` view hp defender
    unit' = view unit defender - killed

effectivePower :: Army -> Int
effectivePower = (*) <$> view (ap . _2) <*> view unit

compareDamage attacker = compare `on` damageDealt attacker

compareSelectAttack attacker a b = compareDamage attacker b a <> compareEffect b a <> compareInitiative b a

compareEffect = compare `on` effectivePower

compareInitiative = compare `on` view initiative

compareSelection a b = compareEffect b a <> compareInitiative b a

damageDealt :: Army -> Army -> Int
damageDealt attacker defender
  | ((==) `on` view system) attacker defender || aType `Set.member` view immune defender = 0
  | aType `Set.member` view weakness defender = 2 * aPoint
  | otherwise = aPoint
  where
    (aType, a) = view ap attacker
    aPoint = effectivePower attacker

parseArmy :: System -> Parser Army
parseArmy s = do
  u <- signedInteger
  string " units each with "
  h <- signedInteger
  string " hit points "
  (imm, wea) <- fromMaybe (Set.empty, Set.empty) <$> optional parseSpecialty
  string "with an attack that does "
  a <- signedInteger <* space
  aType <- some (satisfy isAlpha)
  string " damage at initiative "
  Army s u h (aType, a) wea imm <$> signedInteger

day24 :: IO ()
day24 = do
  Just input <- parseMaybe parseInput <$> readFile "input/input24.txt"
  -- Just input <- parseMaybe parseInput <$> readFile "input/test24.txt"
  let input' = map (\n -> fmap (\x -> if view system x == Immune then over (ap . _2) (+ n) x else x) input) [1 ..]
      a f i = do
        (_, output) <- firstRepeat' (iterate round i)
        guard (f output)
        return output
  print $
    sum
      . fmap (view unit)
      <$> a ((||) <$> all ((== Immune) . view system) <*> all ((== Infection) . view system)) input
  print $ sum . fmap (view unit) <$> join (find isJust $ map (a (all ((== Immune) . view system))) input')
