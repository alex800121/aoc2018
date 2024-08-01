module Day9 where

import Control.Monad.ST (runST)
import Control.Monad.ST.Strict (ST)
import Data.Maybe (fromJust)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.STRef.Strict (STRef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Paths_AOC2018
import Control.Arrow (Arrow(second, first))
import Debug.Trace (traceShow, traceM)
-- import Data.CircularList

playerN = 452

lastMarble = 70784

playGame :: Int -> Int -> V.Vector Int
playGame nP nM = runST $ do
  s <- M.replicate nP (0 :: Int)
  c <- M.new (nM + 1) 
  M.write c 1 (0, 0)
  let step f [] = pure ()
      step f (x : xs)
        | x `mod` 23 == 0 = do
            f' <- shiftN fst f 7
            (prev, next) <- M.read c f'
            M.modify s (+ (f' + x)) (x `mod` nP)
            M.modify c (second (const next)) prev
            M.modify c (first (const prev)) next
            step next xs
        | otherwise = do
            (_, prev) <- M.read c f
            (_, next) <- M.read c prev
            M.write c x (prev, next) 
            M.modify c (second (const x)) prev
            M.modify c (first (const x)) next
            step x xs
      shiftN f y n
        | n <= 0 = pure y
        | otherwise = do
            x <- M.read c y
            shiftN f (f x) (n - 1)
  step 0 [1 .. nM]
  V.freeze s

showV i v = go i
  where
    go n
      | i == i' = [n]
      | otherwise = n : go i'
      where
        (_, i') = v V.! n

-- playGame :: Int -> Int -> V.Vector Int
-- playGame nP nM = runST $ do
--   s <- M.replicate nP (0 :: Int)
--   c <- newSTRef (Data.CircularList.singleton 0)
--   let step i
--         | i `mod` 23 == 0 = do
--             modifySTRef' c (rotNL 7)
--             t <- fromJust . focus <$> readSTRef c
--             M.modify s (+ (i + t)) (i `mod` nP)
--             modifySTRef' c removeR
--         | otherwise = modifySTRef' c (insertL i . rotR)
--   mapM_ step [1 .. nM]
--   V.freeze s

day9 :: IO ()
day9 = do
  putStrLn
    . ("day9a: " ++)
    . show
    . V.maximum
    $ playGame playerN lastMarble
  putStrLn
    . ("day9a: " ++)
    . show
    . V.maximum
    $ playGame playerN (lastMarble * 100)
