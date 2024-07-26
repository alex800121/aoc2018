module Day2 where

import Control.Arrow
import Data.List (elem, group, sort)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Paths_AOC2018
import MyLib (pickAnySplit)
import Control.Applicative (asum)

correct :: String -> String -> Maybe String
correct a b
  | length a == length c + 1 = Just c
  | otherwise = Nothing
  where
    c = catMaybes $ zipWith (\x y -> if x == y then Just x else Nothing) a b

day2 :: IO ()
day2 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  let a = map (sort >>> group >>> map length) input
  putStrLn
    . ("day2a: " ++)
    . show
    . uncurry (*)
    $ (length . filter (elem 2) &&& length . filter (elem 3)) a
  putStrLn
    . ("day2b: " ++)
    . fromMaybe "No answer"
    . asum
    . map (\(x, xs) -> asum (map (correct x) xs))
    $ pickAnySplit input
