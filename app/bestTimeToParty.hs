module Main where

import BestTimeToParty

main :: IO ()
main = interact (bestTimeToParty . readSchedule)

readSchedule :: String -> [Schedule]
readSchedule s = case readList s of
  [(sched,_)] -> sched
  _           -> error "invalid string"
