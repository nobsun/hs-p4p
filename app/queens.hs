module Main where

import System.Environment
import KeepThoseQueensApart

main :: IO ()
main = do
  { args <- getArgs
  ; case args of
      []  -> display 8
      n:_ -> display (read n)
  }

display :: Int -> IO ()
display n =  putStr . unlines . map (unlines . map (showQueens n)) $ nqueens n
