module YouWillAllConform where

type Cap = Char
type Cmd = String

pleaseConform :: [Cap] -> [Cmd]
pleaseConform = map mkCmd . pickupRanges . makeRanges . group

type Pos = Int
type Range = (Pos, Pos)

mkCmd :: Range -> Cmd
mkCmd (i, j)
  | i == j    = showPos i ++ change
  | otherwise = showPos i ++ "から" ++ showPos j ++ change
  where
    showPos pos = show pos ++ "番目"
    change      = "の人は帽子の向きを替えてください"

group :: [Cap] -> [[Cap]]
group []     = []
group (c:cs) = case span (c ==) cs of
  (xs, ys) -> (c:xs) : group ys

makeRanges :: [[Cap]] -> [Range]
makeRanges = mkRanges 0
  where
    mkRanges _ []       = []
    mkRanges i (xs:xss) = (i, pred j) : mkRanges j xss
      where
        j = i + length xs

pickupRanges :: [Range] -> [Range]
pickupRanges []       = []
pickupRanges [_]      = []
pickupRanges (_:r:rs) = r : pickupRanges rs
