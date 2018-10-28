module BestTimeToParty where

import Data.Bool
import Data.Function (on)
import Data.List (maximumBy, sort, unfoldr)
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
makeTimingSheet = unfoldr psi . (,) 0 . sort . concatMap arriveOrLeave
  where
    psi (_,[]) = Nothing
    psi (c,(t,al):xs) = case spanCountUpDown (t ==) c xs of
      (c',ys) -> Just ((t,c''), (c'',ys))
        where
          c'' = bool pred succ al c'

spanCountUpDown :: Enum b => (a -> Bool) -> b -> [(a, Bool)] -> (b, [(a, Bool)]) 
spanCountUpDown p c = para phi (c,[])
  where
    phi x@(t,al) (xs, y)
      | p t       = case y of (c',ys) -> (bool pred succ al c', ys)
      | otherwise = (c,x:xs)

para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para phi z [] = z
para phi z (x:xs) = phi x (xs, para phi z xs)

arriveOrLeave :: Schedule -> [(Time, Bool)]
arriveOrLeave (a, l) = [(a, True), (l, False)]
