module Main where

import Control.Monad.State
import System.Environment
import System.IO
import BreakTheCrystal

main :: IO ()
main = do
  { hSetBuffering stdout NoBuffering
  ; n:d:_ <- getArgs
  ; evalStateT experiment (initState (read n) (read d))
  }
