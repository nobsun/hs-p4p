{-# LANGUAGE NPlusKPatterns #-}
module KeepThoseQueensApart where

nqueens :: Int -> [[Int]]
nqueens n = queens n n

queens :: Int -> Int -> [[Int]]
queens m n = case n of
  0    -> [[]]
  n'+1 -> [ p ++ [n''] | p <- queens m n', n'' <- [1 .. m], safe p (succ n') n'' ]

safe :: [Int] -> Int -> Int -> Bool
safe p m n = all not [ conflict (i,j) (m,n) | (i,j) <- zip [1..] p]

conflict :: (Int, Int) -> (Int, Int) -> Bool
conflict (i,j) (m,n) = j == n || i + j == m + n || i - j == m - n

blanks :: Int -> String
blanks = flip replicate '・'

showQueens :: Int -> Int -> String
showQueens n r = case splitAt (pred r) bs of
  (ss,_:ts) -> ss ++ 'Ｑ':ts
  where
    bs = blanks n

