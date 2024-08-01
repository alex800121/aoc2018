module Day9 where

import Paths_AOC2018
import Data.Function (fix)

data CircList a = CircNode
  { _prev :: CircList a,
    _node :: a,
    _next :: CircList a
  }

instance Show a => Show (CircList a) where
  show (CircNode _ n _) = ".. : " ++ show n ++ " : .."

mkCircList :: [a] -> CircList a
mkCircList [] = error "must have one node"
mkCircList (x : xs) = last
  where
    (last, prev) = go prev x xs last
    go p x [] r = let l = CircNode p x r in (l, l)
    go p x (y : ys) r = (l', last)
      where
        (r', last) = go l' y  ys r
        l' = CircNode p x r'

insert :: a -> CircList a -> CircList a
insert x (CircNode l a r) = fix $ \n -> CircNode (CircNode l a n) x r
-- >>> l = mkCircList [1,2,3,4,5]
-- >>> iterate _prev l !! 99
-- .. : 2 : ..

day9 :: IO ()
day9 = do
  -- input <- (getDataDir >>= readFile . (++ "/input/input9.txt"))
  return ()
