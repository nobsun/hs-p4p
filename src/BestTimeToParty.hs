module BestTimeToParty where

import Data.List (maximumBy)
import Data.Ord (comparing)

type Time     = Int
type Schedule = (Time, Time)

bestTimeToParty :: [Schedule] -> String
bestTimeToParty = mkMsg . bestTiming . timingSheet

mkMsg :: (Time, Int) -> String
mkMsg (t, n) = "Best time to attend the party is at "
            ++ show t ++ " o'clock : "
            ++ show n ++ " celebrities will be attending!"

bestTiming :: [(Time, Int)] -> (Time, Int)
bestTiming = maximumBy (comparing snd)

timingSheet :: [Schedule] -> [(Time, Int)]
timingSheet = mkTmSheet . concatMap arriveLeave

mkTmSheet :: [(Time, Bool)] -> [(Time, Int)]
mkTmSheet = undefined

arriveLeave :: Schedule -> [(Time, Bool)]
arriveLeave = undefined
