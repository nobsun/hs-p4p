module Main where

import Control.Monad.State
import System.Environment
import BreakTheCrystal

main :: IO ()
main = do
  { n:d:_ <- getArgs
  ; evalStateT experiment (initState (read n) (read d))
  }
