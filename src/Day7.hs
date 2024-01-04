module Day7 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace
import Data.Char (ord)
import Data.List (sort, minimumBy, partition)
import Data.Function (on)
import Data.Bifunctor (Bifunctor(..))

type D = Map Char (Set Char)

parseDepend :: Parser D
parseDepend = do
  a <- string "Step " >> anySingle <* string " must be finished before step "
  b <- anySingle <* string " can begin."
  return $ Map.singleton b (Set.singleton a)

day7a :: D -> String
day7a = f Set.empty
  where
    f candidate d
      -- | traceShow (Set.union candidate $ Map.keysSet c) False = undefined
      | Map.null d = Set.toList candidate
      | otherwise = c' : f candidate' (Map.map (Set.delete c') rest)
      where
        (c, rest) = Map.partition Set.null d
        (c', candidate') = Set.deleteFindMin $ Set.union candidate $ Map.keysSet c

day7b :: Int -> Int -> D -> Int
day7b n added = f 0 [] Set.empty
  where
    calcTime c = added + 1 + ord c - ord 'A'
    f acc workers candidates d
      -- | traceShow (w, candidates', workers', acc) False = undefined
      | Set.null candidates && Map.null d = acc + foldr (max . snd) 0 workers
      | otherwise = f (acc + t) workers'' candidates' (Map.map (Set.\\ (Set.fromList $ map fst w')) rest)
      where
        (c, rest) = Map.partition Set.null d
        (w, candidates') = Set.splitAt (n - length workers) $ Set.union candidates $ Map.keysSet c
        workers' = workers <> map ((,) <$> id <*> calcTime) (Set.toList w)
        t = snd $ minimumBy (compare `on` snd) workers'
        (workers'', w') = partition ((> 0) . snd) $ map (second (subtract t)) workers'

day7 :: IO ()
day7 = do
  input <- Map.unionsWith (<>) . mapMaybe (parseMaybe parseDepend) . lines <$> readFile "input/input7.txt"
  -- input <- Map.unionsWith (<>) . mapMaybe (parseMaybe parseDepend) . lines <$> readFile "input/test7.txt"
  let start = Set.unions (Map.elems input) Set.\\ Map.keysSet input
      input' = Map.union input (Map.fromSet (const Set.empty) start)
  putStrLn $ day7a input'
  print $ day7b 5 60 input'
  -- print $ day7b 2 0 input'
