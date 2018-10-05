module YouWillAllConform where

type Cap = Char
type Cmd = String

pleaseConform :: [Cap] -> [Cmd]
pleaseConform = map mkCmd . pickupRanges . makeRanges

type Pos = Int
type Range = (Pos, Pos)

mkCmd :: Range -> Cmd
mkCmd (i, j)
  | i == j    = showPos i ++ change
  | otherwise = showPos i ++ "から" ++ showPos j ++ change
  where
    showPos pos = show pos ++ "番目"
    change      = "の人は帽子の向きを替えてください"

pickupRanges :: [Range] -> [Range]
pickupRanges []       = []
pickupRanges [_]      = []
pickupRanges (_:r:rs) = r : pickupRanges rs

makeRanges :: [Cap] -> [Range]
makeRanges = mkRanges 0
  where
    mkRanges _ []     = []
    mkRanges i (c:cs) = case spanCount (c ==) cs of
      (m, cs') -> (i,j) : mkRanges (succ j) cs'
        where
          j = i + m
    spanCount _ [] = (0, [])
    spanCount p xxs@(x:xs)
      | p x       = case spanCount p xs of (n, xs') -> (succ n, xs')
      | otherwise = (0, xxs)
