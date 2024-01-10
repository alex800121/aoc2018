module Day20 where

import MyLib
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Char (isAlpha)

type Index = (Int, Int)

unit :: Parser [String]
unit = do
  a <- many (divergence <|> direction)
  return $ foldr (\x acc -> undefined) [] a

direction = many (satisfy isAlpha)

divergence = do
  char '('
  a <- unit `sepBy` char '|'
  char ')'
  return a

day20 :: IO ()
day20 = do
  input <- init <$> readFile "input/input20.txt"
  print input
