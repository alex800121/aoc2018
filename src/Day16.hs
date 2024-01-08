module Day16 where

import Data.Bits (Bits (..))
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import MyLib
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.List.Split (splitOn)

data Test = Test
  { _before :: Vector Int,
    _instruction :: (Int, Vector Int),
    _after :: Vector Int
  }
  deriving (Show, Eq, Ord)

{- 
Before: [0, 1, 2, 0]
3 1 0 0
After:  [1, 1, 2, 0] 
-}
testParser :: Parser Test
testParser = do
  string "Before: ["
  a <- signedInteger `sepBy` string ", "
  char ']' >> newline
  b <- signedInteger `sepBy` char ' '
  newline
  string "After: ["
  c <- signedInteger `sepBy` string ", "
  char ']'
  return $ Test (V.fromList a) (head b, V.fromList $ tail b) (V.fromList c)

interpretWith :: Vector OpCode -> Int -> Vector Int -> Vector Int -> Vector Int
interpretWith opV op = opcode (opV ! op)

validOpCode :: Test -> [OpCode]
validOpCode (Test b i a) = filter (\x -> opcode x (snd i) b == a) [minBound .. maxBound]

opcode :: OpCode -> Vector Int -> Vector Int -> Vector Int
opcode op v0 v = v // [(c, result)]
  where
    a = v ! 0
    b = v ! 1
    c = v ! 2
    regA = v ! a
    regB = v ! b
    result = case op of
      Addr -> regA + regB
      Addi -> regA + b
      Mulr -> regA * regB
      Muli -> regA * b
      Banr -> regA .&. regB
      Bani -> regA .&. b
      Borr -> regA .|. regB
      Bori -> regA .|. b
      Setr -> regA
      Seti -> a
      Gtir -> if a > regB then 1 else 0
      Gtri -> if regA > b then 1 else 0
      Gtrr -> if regA > regB then 1 else 0
      Eqir -> if a == regB then 1 else 0
      Eqri -> if regA == b then 1 else 0
      Eqrr -> if regA == regB then 1 else 0

data OpCode
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Show, Eq, Ord, Enum, Bounded)

day16 :: IO ()
day16 = do
  [a, b] <- splitOn "\n\n\n" <$> readFile "input/input16.txt"
  let tests = parseMaybe (testParser `sepBy` string "\n\n") a
  print tests
