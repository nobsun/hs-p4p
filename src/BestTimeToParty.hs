module BestTimeToParty where

type Time     = Int
type Schedule = (Time, Time)
type Timing   = (Time, Int)
type Message  = String

bestTimeToParty :: [Schedule] -> Message
bestTimeToParty = makeMessage . gonyogonyo

makeMessage :: Timing -> Message
makeMessage (t, n) = "Best time to attend the party is at "
                  ++ show t ++ " o'clock : "
                  ++ show n ++ " celebrities will be attending!"

gonyogonyo :: [Schedule] -> Timing
gonyogonyo = undefined
