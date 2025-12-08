module Day21 where

import Data.Bits (Bits (..))
import Data.IntSet qualified as IS
import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import Paths_AOC2018
import Text.Read (readMaybe)

buildList m0 seed = unfoldr loopB (0 .|. m0, seed)
  where
    loopB (e, f)
      | 256 > e = Just (f', (f' .|. m0, seed))
      | otherwise = loopB (e', f')
      where
        e' = e `shiftR` 8
        f' = (((f + (e .&. 255)) .&. 16777215) * 65899) .&. 16777215

getLastRepeat (x : xs) = go IS.empty x xs
  where
    go s l [] = l
    go s l (x : xs)
      | x `IS.member` s' = l
      | otherwise = go s' x xs
      where
        s' = IS.insert l s

day21 :: IO (String, String)
day21 = do
  input <- concatMap (mapMaybe (readMaybe @Int) . words) . lines <$> (getDataDir >>= readFile . (++ "/input/input21.txt"))
  let seed = input !! 22
      m0 = input !! 20
      l = buildList m0 seed
  let
    !finalAnsa =
      show $
        head l
  let
    !finalAnsb =
      show $
        getLastRepeat l
  pure (finalAnsa, finalAnsb)
