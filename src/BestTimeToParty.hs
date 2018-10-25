module BestTimeToParty where

import Data.List (maximumBy)
import Data.Ord (comparing)

type Time     = Int
type Schedule = (Time, Time)
type Timing   = (Time, Int)
type Message  = String

bestTimeToParty :: [Schedule] -> Message
bestTimeToParty = makeMessage . bestTiming . timingSheet

makeMessage :: (Time, Int) -> Message
makeMessage (t, n) = "Best time to attend the party is at "
                  ++ show t ++ " o'clock : "
                  ++ show n ++ " celebrities will be attending!"

bestTiming :: [(Time, Int)] -> (Time, Int)
bestTiming = maximumBy (comparing snd)

timingSheet :: [Schedule] -> [(Time, Int)]
timingSheet = makeTimingSheet . concatMap arriveOrLeave

makeTimingSheet :: [(Time, Bool)] -> [(Time, Int)]
makeTimingSheet = undefined

arriveOrLeave :: Schedule -> [(Time, Bool)]
arriveOrLeave = undefined
