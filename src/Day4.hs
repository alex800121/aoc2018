{-# LANGUAGE LambdaCase #-}

module Day4 where

import Control.Applicative (Alternative (..))
import Control.Arrow
import Data.Function (on)
import Data.Functor (($>))
import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as MS
import Data.List (maximumBy, sort)
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import MyLib (Parser, signedInteger)
import Paths_AOC2018
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, space1, string)

data Event = Shift Int | Sleep | Wake deriving (Show, Eq, Ord)

data Time = Time {month :: Int, day :: Int, minute :: Int} deriving (Show, Eq, Ord)

type Guard = Map Int IntMultiSet

type TimeEvent = (Time, Event)

parseTimeEvent :: Parser TimeEvent
parseTimeEvent = do
  m <- string "[1518-" *> signedInteger
  d <- char '-' *> signedInteger
  h <- space1 *> signedInteger
  minute <- char ':' *> signedInteger
  let time = if h /= 0 then Time m (d + 1) (minute - 60) else Time m d minute
  event <-
    string "] "
      *> ( (string "wakes up" $> Wake)
             <|> (string "falls asleep" $> Sleep)
             <|> Shift
             <$> (string "Guard #" *> signedInteger <* string " begins shift")
         )
  pure (time, event)

buildGuard :: [TimeEvent] -> Maybe Guard
buildGuard xs
  | (_, Shift i) : xs <- xs = M.singleton i <$> go xs
  | otherwise = Nothing
  where
    go [] = Just MS.empty
    go ((Time _ _ m0, Sleep) : (Time _ _ m1, Wake) : xs) = MS.union (MS.fromList [m0 .. m1 - 1]) <$> go xs
    go _ = Nothing

day4 :: IO ()
day4 = do
  input <-
    M.unionsWith MS.union
      . mapMaybe buildGuard
      . split (keepDelimsL $ whenElt (\case (_, Shift _) -> True; _ -> False))
      . sort
      . mapMaybe (parseMaybe parseTimeEvent)
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input4.txt"))
  putStrLn
    . ("day4a: " ++)
    . show
    . (second (fst . maximumBy (compare `on` snd) . MS.toOccurList) >>> uncurry (*))
    . maximumBy (compare `on` (sum . map snd . MS.toOccurList . snd))
    . M.toList
    $ input
  putStrLn
    . ("day4b: " ++)
    . show
    . (second fst >>> uncurry (*))
    . maximumBy (compare `on` snd . snd)
    . map (second (maximumBy (compare `on` snd)))
    . filter (not . null . snd)
    . map (second MS.toOccurList)
    $ M.toList input
