module YouWillAllConform where

import Control.Arrow (first)
import Data.List (unfoldr)

type Cap = Char
type Cmd = String

pleaseConform :: [Cap] -> [Cmd]
pleaseConform = unfoldr psi . (,) 0
  where
    psi (_, [])   = Nothing
    psi (i, c:cs) = case spanCount (c ==) cs of
      (_, [])   -> Nothing
      (m, d:ds) -> case spanCount (d ==) ds of
        (n, es)   -> Just (mkCmd (j, k), (succ k, es))
          where
            j = i + succ m
            k = j + n

spanCount :: (a -> Bool) -> [a] -> (Int, [a])
spanCount p = para phi (0,[])
  where
    phi x (xs, mys)
      | p x       = first succ mys
      | otherwise = (0, x:xs)

para :: (a -> ([a],b) -> b) ->  b -> [a] -> b
para phi z []     = z
para phi z (x:xs) = phi x (xs, para phi z xs)

type Pos = Int
type Range = (Pos, Pos)

mkCmd :: Range -> Cmd
mkCmd (i, j)
  | i == j    = showPos i ++ change
  | otherwise = showPos i ++ "から" ++ showPos j ++ change
  where
    showPos pos = show pos ++ "番目"
    change      = "の人は帽子の向きを替えてください"

