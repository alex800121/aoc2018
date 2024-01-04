module Day1 where
import Text.Megaparsec
import MyLib
import Data.Maybe (mapMaybe)
import Data.List (scanl')

f (x : xs) acc = acc : f xs (acc + x)
day1 :: IO ()
day1 = do
  input <- mapMaybe (parseMaybe signedInteger) . lines <$> readFile "input/input1.txt"
  print $ sum input
  print $ fmap snd $ firstRepeat' $ scanl (+) 0 $ cycle input
