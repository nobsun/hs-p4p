module YouWillAllConform where

type Cap = Char
type Cmd = String

pleaseConform :: [Cap] -> [Cmd]
pleaseConform = mkRanges 0
  where
    mkRanges _ []     = []
    mkRanges i (c:cs) = case spanCount (c ==) cs of
      (m, [])   -> []
      (m, d:ds) -> case spanCount (d ==) ds of
        (n, es)   -> mkCmd (j, k) : mkRanges (succ k) es
          where
            j = i + succ m
            k = j + n
    spanCount _ [] = (0, [])
    spanCount p xxs@(x:xs)
      | p x       = case spanCount p xs of (n, xs') -> (succ n, xs')
      | otherwise = (0, xxs)


type Pos = Int
type Range = (Pos, Pos)

mkCmd :: Range -> Cmd
mkCmd (i, j)
  | i == j    = showPos i ++ change
  | otherwise = showPos i ++ "から" ++ showPos j ++ change
  where
    showPos pos = show pos ++ "番目"
    change      = "の人は帽子の向きを替えてください"
