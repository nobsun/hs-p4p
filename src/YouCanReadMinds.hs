module YouCanReadMinds where

import Control.Arrow ((***))
import Data.Bool (bool)
import Data.Function (on)
import Data.List (sort, groupBy, partition, permutations, findIndex, unfoldr, intercalate)
import qualified Data.Map as M

type Card  = (Suite, Int)
data Suite = C | D | H | S deriving (Eq, Ord, Enum, Show, Read)

showCard :: Card -> String
showCard (s, n)  = (case n of
  { 1  -> ("A_" ++)
  ; 11 -> ("J_" ++)
  ; 12 -> ("Q_" ++)
  ; 13 -> ("K_" ++)
  ; _  -> (show n ++) . ('_':)
  } ) (show s)

showDeck :: [Card] -> String
showDeck = unlines
         . map (intercalate " ")
         . splitEvery 4 . map showCard

splitEvery n = unfoldr psi
  where
    psi x = case x of
      [] -> Nothing
      _  -> Just $ splitAt n x

readCard :: String -> Card
readCard s = case break ('_' ==) s of
  (xs,_:ys) -> case xs of
    "A" -> (read ys, 1)
    "J" -> (read ys, 11)
    "Q" -> (read ys, 12)
    "K" -> (read ys, 13)
    _   -> (read ys, read xs)

deck :: [Card]
deck = [ (s, n) | n <- [1 .. 13] , s <- [C .. S] ]

order :: [Card] -> [Card]
order = gather
      . (concat *** pickup)
      . partition singleton
      . groupBy ((==) `on` fst)
      . sort

singleton :: [a] -> Bool
singleton [_] = True
singleton _   = False

pickup :: [[Card]] -> (Int, [Card])
pickup ((sx@(_,x):sy@(_,y):cs) : css) = bool (d,(sx:cs)++concat css) (13-d, (sy:cs) ++ concat css) (6 < d)
  where
    d = y - x

gather :: ([Card], (Int, [Card])) -> [Card]
gather (xs, (n, y:ys)) = y : (permutations (sort (ys++xs)) !! (n-1))

sample :: [Card]
sample = [(H, 10), (D, 9), (H, 3), (S, 12), (D, 11)]

guess :: [Card] -> Card
guess ((s,n):xs) = case findIndex (xs ==) (permutations (sort xs)) of
  Just m -> (s, bool n' (n'-13) (n' > 13))
    where
      n' = n + m + 1
