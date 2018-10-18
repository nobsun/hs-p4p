module BestTimeToParty where

type Time     = Int
type Schedule = (Time, Time)

bestTimeToParty :: [Schedule] -> String
bestTimeToParty = mkMsg . gonyogonyo

mkMsg :: (Time, Int) -> String
mkMsg (t, n) = "Best time to attend the party is at "
            ++ show t ++ " o'clock : "
            ++ show n ++ " celebrities will be attending!"

gonyogonyo :: [Schedule] -> (Time, Int)
gonyogonyo = undefined
