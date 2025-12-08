module Day19 where

import Data.Char (toUpper)
import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as M
import Day16
import MyLib (factors)
import Paths_AOC2018

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
  Just v' -> v : run insReg vop (V.modify (\x -> M.modify x (+ 1) insReg) v')

parseInput = ((,) <$> parseReg . head <*> V.fromList . map parseOpCode . tail) . lines

parseReg = read @Int . last . words

initVec = V.fromList (replicate 6 0)

day19 :: IO (String, String)
day19 = do
  input <- parseInput <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  -- input <- parseInput <$> readFile "input/test19.txt"
  let ansA = (! 4) $ (!! 20) $ uncurry run input initVec
      ansB = (! 4) $ (!! 20) $ uncurry run input $ V.modify (\x -> M.write x 0 1) initVec
  let
    !finalAnsa =
      show
        . sum
        $ factors ansA
  let
    !finalAnsb =
      show
        . sum
        $ factors ansB
  pure (finalAnsa, finalAnsb)
