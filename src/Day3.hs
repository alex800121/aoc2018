module Day3 where

import Data.Foldable (Foldable (..), find)
import Data.Maybe (mapMaybe)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Debug.Trace (traceShow)
import MyLib (Nat (..), Parser, Vec (..), jointEucVecs, jointEucVecs', overlapEucVec, signedInteger, subtractEucVecs')
import Paths_AOC2018
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, string)

type F = MultiSet (Int, Int)

data Fabric = Fabric {fid :: Int, fabric :: F}
  deriving (Show, Eq, Ord)

parseFabric :: Parser Fabric
parseFabric = do
  i <- char '#' *> signedInteger
  a <- string " @ " *> signedInteger
  b <- char ',' *> signedInteger
  c <- string ": " *> signedInteger
  d <- char 'x' *> signedInteger
  pure $ Fabric i $ MS.fromList [(x, y) | x <- [a .. a + c - 1], y <- [b .. b + d - 1]]

day3 :: IO ()
day3 = do
  input <- mapMaybe (parseMaybe parseFabric) . lines <$> (getDataDir >>= readFile . (++ "/input/input3.txt"))
  let o = MS.unions $ map fabric input
  putStrLn
    . ("day3a: " ++)
    . show
    . length
    . filter ((> 1) . snd)
    $ MS.toOccurList o
  putStrLn
    . ("day3b: " ++)
    . show
    . fmap fid
    . find (all ((== 1) . flip MS.occur o) . fabric)
    $ input
