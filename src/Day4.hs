module Day4 where

import Data.List (sort, maximumBy, group)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Data.Bifunctor (Bifunctor(..))

data Hint = Guard Int | Sleep | Wake deriving (Show, Ord, Eq)

parseHint :: Parser (Int, Hint)
parseHint = do
  count (length "[1518-XX-XX ") anySingle
  a <- signedInteger
  char ':'
  b <- signedInteger
  string "] "
  c <- (string "falls asleep" >> return Sleep) <|> (string "wakes up" >> return Wake) <|> (string "Guard #" >> Guard <$> signedInteger <* string " begins shift")
  return (if a > 21 then b - 60 else b, c)

buildTimeTable :: [(Int, Hint)] -> Map Int Sleep
buildTimeTable [] = Map.empty
buildTimeTable (g@(_, Guard i) : (a, Sleep) : (b, Wake) : xs) = Map.insertWith (<>) i [(a, b)] $ buildTimeTable (g : xs)
buildTimeTable ((_, Guard _) : xs) = buildTimeTable xs

type Guard = (Int, [Sleep])

type Sleep = [Cycle]

type Cycle = (Int, Int)

day4 :: IO ()
day4 = do
  input' <- sort . lines <$> readFile "input/input4.txt"
  let input = buildTimeTable $ mapMaybe (parseMaybe parseHint) input'
  -- putStrLn $ unlines input'
  print $ uncurry (*) $ second (head . maximumBy (compare `on` length) . group . sort . concatMap (\(x, y) -> [x .. y - 1])) $ maximumBy (compare `on` (sum . map (uncurry subtract) . snd)) $ Map.toList input
  print $ uncurry (*) $ second head $ maximumBy (compare `on` length . snd) $ map (second (maximumBy (compare `on` length) . group . sort . concatMap (\(x, y) -> [x .. y - 1]))) $ Map.toList input
