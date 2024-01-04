module Day9 where

import Control.Monad.ST.Strict
import Data.Bifunctor (Bifunctor (..))
import Data.List (transpose, unfoldr)
import Data.List.Split (chunksOf)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Debug.Trace

player = 452

marble = 70784

-- player = 10; marble = 1618
-- player = 13; marble = 7999
-- player = 17; marble = 1104
-- player = 21; marble = 6111
-- player = 30; marble = 5807

type Chain = (Int, Seq Int)

type Chain' s = M.STVector s (Int, Int)

type Score s = M.STVector s Int

type GameState s = (Score s, Chain' s)

run :: Int -> Int -> Int -> Int -> Int -> GameState s -> ST s ()
run limit players currentMarble currentPlayer currentPos g@(score, vec)
  -- | traceShow (currentPlayer, currentMarble, currentPos) False = undefined
  | currentMarble > limit = return ()
  | currentMarble `mod` 23 == 0 = do
      removed <- backward 7 currentPos vec
      next <- deleteChain removed vec
      M.modify score (+ (removed + currentMarble)) currentPlayer
      run limit players nextMarble nextPlayer next (score, vec)
  | otherwise = do
      next <- snd <$> M.read vec currentPos
      insertChain currentMarble next vec
      run limit players nextMarble nextPlayer currentMarble (score, vec)
  where
    nextPlayer = (currentPlayer + 1) `mod` players
    nextMarble = currentMarble + 1

forward, backward :: Int -> Int -> Chain' s -> ST s Int
forward n pos c
  | n == 0 = pure pos
  | n > 0 = do
      pos' <- snd <$> M.read c pos 
      forward (n - 1) pos' c
  | n < 0 = do
      pos' <- fst <$> M.read c pos
      forward (n + 1) pos' c
backward n = forward (negate n)

insertChain :: Int -> Int -> Chain' s -> ST s ()
insertChain n pos c = do
  next <- snd <$> M.read c pos
  M.modify c (second (const n)) pos
  M.modify c (first (const n)) next
  M.write c n (pos, next)

deleteChain :: Int -> Chain' s -> ST s Int
deleteChain n c = do
  (prev, next) <- M.read c n
  M.modify c (second (const next)) prev
  M.modify c (first (const prev)) next
  return next

run' limit players = runST $ do
  score <- M.replicate players 0
  vec <- M.new (limit + 1)
  run limit players 1 0 0 (score, vec)
  (,) <$> U.freeze score <*> U.freeze vec

next :: Int -> (Int, Chain) -> Maybe (Int, (Int, Chain))
next limit (n, (pos, c))
  -- \| traceShow l False = undefined
  | n > limit = Nothing
  | n `mod` 23 == 0 = Just (score, (n + 1, ans0))
  | otherwise = Just (0, (n + 1, ans1))
  where
    l = length c
    score = n + c `S.index` pos7
    pos7 = (pos - 7) `mod` l
    ans0 = (pos7, S.deleteAt pos7 c)
    pos' = (pos + 2) `mod` l
    ans1 = (pos', S.insertAt pos' n c)

reconstruct :: U.Vector (Int, Int) -> [Int]
reconstruct = f 0
  where
    f n v
      | next == 0 = [n]
      | otherwise = n : f next v
      where
        next = snd $ v U.! n

day9 :: IO ()
day9 = do
  -- print $ maximum $ map sum $ transpose $ chunksOf player $ unfoldr (next marble) (1, (0, S.singleton 0))
  -- print $ maximum $ map sum $ transpose $ chunksOf player $ unfoldr (next (100 * marble)) (1, (0, S.singleton 0))
  print $ U.maximum $ fst $ run' marble player
  print $ U.maximum $ fst $ run' (marble * 100) player
