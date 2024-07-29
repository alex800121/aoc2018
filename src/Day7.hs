module Day7 where

import Control.Arrow
import Control.Monad.ST (runST)
import Data.Char (chr, ord)
import Data.Foldable (Foldable (..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (partition)
import Debug.Trace (traceShow)
import Paths_AOC2018

-- Step G must be finished before step M can begin.

parseStep :: [String] -> IntMap IntSet
parseStep = foldl' f (IM.fromList [(k, IS.empty) | k <- [1 .. 26]])
  where
    f v xs = IM.insertWith (<>) y (IS.singleton x) v
      where
        _ : a : _ : _ : _ : _ : _ : b : _ = words xs
        [x, y] = map (head >>> ord >>> subtract (ord 'A') >>> (+ 1)) [a, b]

buildSeq :: IntMap IntSet -> [Int]
buildSeq im = case IM.minViewWithKey now of
  Nothing -> []
  Just ((k, _), _) -> k : buildSeq (IM.map (IS.filter (/= k)) $ IM.delete k im)
  where
    now = IM.filter IS.null im

calcTime :: Int -> Int -> IntMap IntSet -> Int
calcTime maxN addedTime = go 0 []
  where
    go t workers im
      | IM.null im = t + foldl' (\a -> max a . snd) 0 workers
      | otherwise = go t' workers'' im''
      where
        (now, im') = first IM.keys $ IM.partition IS.null im
        workN = length workers
        needN = maxN - workN
        (canWork, wait) = splitAt needN now
        workers' = workers <> map (id &&& (+ addedTime)) canWork
        minT = minimum $ map snd workers'
        t' = t + minT
        work = map (second (subtract minT)) workers'
        (done, workers'') = first (map fst) $ partition ((<= 0) . snd) work
        im'' = IM.fromSet (const IS.empty) (IS.fromList wait) <> IM.map (IS.filter (`notElem` done)) im'

day7 :: IO ()
day7 = do
  input <- parseStep . lines <$> (getDataDir >>= readFile . (++ "/input/input7.txt"))
  let s = buildSeq input
  putStrLn
    . ("day7a: " ++)
    $ map (chr . (+ ord 'A') . subtract 1) s
  putStrLn
    . ("day7a: " ++)
    . show
    $ calcTime 5 60 input
