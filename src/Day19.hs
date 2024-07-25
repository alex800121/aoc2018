module Day19 where

import Paths_AOC2018
import Data.Char (toUpper)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Day16
import MyLib (factors)

type Instruction = (OpCode, Vector Int)

parseOpCode :: String -> Instruction
parseOpCode =
  ((,) <$> read @OpCode . ((:) <$> toUpper . head <*> tail) . head <*> V.fromList . map read . tail)
    . words

interpretWith' :: Int -> Vector Instruction -> Vector Int -> Maybe (Vector Int)
interpretWith' insReg vop v = (\i -> uncurry opcode i v) <$> (vop V.!? (v ! insReg))

run :: Int -> Vector Instruction -> Vector Int -> [Vector Int]
run insReg vop v = case interpretWith' insReg vop v of
  Nothing -> [v]
  Just v' -> v : run insReg vop ( V.modify (\x -> M.modify x (+ 1) insReg) v')

parseInput = ((,) <$> parseReg . head <*> V.fromList . map parseOpCode . tail) . lines

parseReg = read @Int . last . words

initVec = V.fromList (replicate 6 0)
day19 :: IO ()
day19 = do
  input <- parseInput <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  -- input <- parseInput <$> readFile "input/test19.txt"
  let ansA = (! 4) $ (!! 20) $ uncurry run input initVec
      ansB = (! 4) $ (!! 20) $ uncurry run input $ V.modify (\x -> M.write x 0 1) initVec
  print $ sum $ factors ansA
  print $ sum $ factors ansB
  -- print $ (! 0) $ last $ uncurry run input initVec
  -- writeFile "output" ""
  -- mapM_ (appendFile "output" . ('\n' :) . show) ans
