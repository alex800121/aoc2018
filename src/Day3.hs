module Day3 where

import Data.Foldable (Foldable (..), find)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShow)
import MyLib (Nat (..), Parser, Vec (..), jointEucVecs, jointEucVecs', overlapEucVec, signedInteger, subtractEucVecs')
import Paths_AOC2018
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, string)

type F = Map (Int, Int) Int

data Fabric = Fabric {fid :: Int, fabric :: F}
  deriving (Show, Eq, Ord)

parseFabric :: Parser Fabric
parseFabric = do
  i <- char '#' *> signedInteger
  a <- string " @ " *> signedInteger
  b <- char ',' *> signedInteger
  c <- string ": " *> signedInteger
  d <- char 'x' *> signedInteger
  pure $ Fabric i $ Map.fromList [((x, y), 1) | x <- [a .. a + c - 1], y <- [b .. b + d - 1]]

day3 :: IO ()
day3 = do
  input <- mapMaybe (parseMaybe parseFabric) . lines <$> (getDataDir >>= readFile . (++ "/input/input3.txt"))
  let o = Map.unionsWith (+) $ map fabric input
  putStrLn
    . ("day3a: " ++)
    . show
    . length
    . filter ((> 1) . snd)
    $ Map.toList o
  putStrLn
    . ("day3b: " ++)
    . show
    . fmap fid
    . find (all ((== 1) . (o Map.!)) . Map.keysSet . fabric)
    $ input
