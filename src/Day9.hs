module Day9 where

import Control.Monad.ST (runST)
import Control.Monad.ST.Strict (ST)
import Data.CircularList
import Data.Function (fix)
import Data.Maybe (fromJust)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.STRef.Strict (STRef)
import Data.Sequence
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Paths_AOC2018

playerN = 452

lastMarble = 70784

playGame :: Int -> Int -> V.Vector Int
playGame nP nM = runST $ do
  s <- M.replicate nP (0 :: Int)
  c <- newSTRef (Data.CircularList.singleton 0)
  let step i
        | i `mod` 23 == 0 = do
            modifySTRef' c (rotNL 7)
            t <- fromJust . focus <$> readSTRef c
            M.modify s (+ (i + t)) (i `mod` nP)
            modifySTRef' c removeR
        | otherwise = modifySTRef' c (insertL i . rotR)
  mapM_ step [1 .. nM]
  V.freeze s

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
