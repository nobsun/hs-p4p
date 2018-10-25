module BestTimeToParty where

import Data.Function (on)
import Data.List (maximumBy, groupBy, sort)
import Data.Ord (comparing)

type Time     = Int
type Schedule = (Time, Time)
type Timing   = (Time, Int)
type Message  = String

bestTimeToParty :: [Schedule] -> Message
bestTimeToParty = makeMessage . bestTiming . makeTimingSheet

makeMessage :: (Time, Int) -> String
makeMessage (t, n) = "Best time to attend the party is at "
            ++ show t ++ " o'clock : "
            ++ show n ++ " celebrities will be attending!"

bestTiming :: [Timing] -> Timing
bestTiming = maximumBy (comparing snd)

makeTimingSheet :: [Schedule] -> [Timing]
makeTimingSheet = timingSheet . groupBy ((==) `on` fst) . sort . concatMap arriveOrLeave

arriveOrLeave :: Schedule -> [(Time, Bool)]
arriveOrLeave = undefined

timingSheet :: [[(Time, Bool)]] -> [Timing]
timingSheet = undefined
