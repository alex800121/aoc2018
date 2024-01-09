module Day17 where

import qualified Data.Array as A
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Day15 (RIndex, RTuple (..))
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char

data Water = W
  { _front :: Set RIndex,
    _down :: Set RIndex,
    _side :: Set RIndex,
    _still :: Set RIndex
  }
  deriving
    (Show, Ord, Eq)

parseInput :: Parser (Set RIndex)
parseInput = do
  a <- (char 'x' >> pure first) <|> (char 'y' >> pure second)
  char '='
  iA <- signedInteger
  char ',' >> space
  b <- (char 'x' >> pure first) <|> (char 'y' >> pure second)
  char '='
  iB <- signedInteger `sepBy` string ".."
  pure $ Set.fromList [a (const iA) $ b (const iBs) (R (0, 0)) | iBs <- [minimum iB .. maximum iB]]

day17 :: IO ()
day17 = do
  input' <- Set.unions . mapMaybe (parseMaybe parseInput) . lines <$> readFile "input/input17.txt"
  print input'
