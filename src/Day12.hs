module Day12 where

import Data.Array
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
  g : r : _ <- splitOn "\n\n" <$> readFile "input/input12.txt"
  -- g : r : _ <- splitOn "\n\n" <$> readFile "input/test12.txt"
  let initState = map (== '#') $ drop (length "initial state: ") g
      rules = Map.fromList $ map ((\[x, y] -> (x, head y)) . map (map (== '#')) . splitOn " => ") $ lines r
  putStrLn $ show $ sum $ map (snd . fst) $ filter ((&&) <$> ((== 20) . fst . fst) <*> (id . snd)) $ assocs $ buildGen initState 20 rules
  -- putStrLn $ show $ sum $ map (snd . fst) $ filter ((&&) <$> ((== 50000000000) . fst . fst) <*> (id . snd)) $ assocs $ buildGen initState 50000000000 rules
