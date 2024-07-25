module Day12 where

import Paths_AOC2018
import Data.Array
import Data.Function (on)
import Data.List (groupBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Day11 ((!?))

type Rule = Map [Bool] Bool

buildGen :: [Bool] -> Int -> Rule -> Array (Int, Int) Bool
buildGen s gen rule = output
  where
    l = (negate gen, length s + gen)
    f s x
      | x < 0 || x >= length s = Nothing
      | otherwise = Just $ s !! x
    zeroGen = array ((0, fst l), (0, snd l)) [((0, x), fromMaybe False (f s x)) | x <- range l]
    b = ((0, fst l), (gen, snd l))
    output =
      array
        b
        [ ((g, i), p g i)
          | (g, i) <- range b
        ]
    p g i
      | g == 0 = zeroGen ! (g, i)
      | otherwise = fromMaybe False (rule Map.!? li)
      where
        h g i = fromMaybe False (output !? (g, i))
        li = [h (g - 1) x | x <- [i - 2 .. i + 2]]

day12 :: IO ()
day12 = do
  g : r : _ <- splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input12.txt"))
  -- g : r : _ <- splitOn "\n\n" <$> readFile "input/test12.txt"
  let initState = map (== '#') $ drop (length "initial state: ") g
      rules = Map.fromList $ map ((\[x, y] -> (x, head y)) . map (map (== '#')) . splitOn " => ") $ lines r
      n200s =
        map
          ( \x ->
              (fst (fst (head x)), sum $ map (snd . fst) $ filter (id . snd) x)
          )
          . groupBy ((==) `on` (fst . fst))
          . assocs
          $ buildGen initState 200 rules
      n200a = array (0, 200) n200s
      n200 = n200a ! 200
      n199 = n200a ! 199
      d = n200 - n199
      n50000000000 = n200 + d * (50000000000 - 200)
  print $ sum $ map (snd . fst) $ filter ((&&) <$> ((== 20) . fst . fst) <*> snd) $ assocs $ buildGen initState 20 rules
  print n50000000000

-- putStrLn $ show $ sum $ map (snd . fst) $ filter ((&&) <$> ((== 50000000000) . fst . fst) <*> (id . snd)) $ assocs $ buildGen initState 50000000000 rules
