module YouWillAllConform where

type Cap = Char
type Cmd = String

pleaseConform :: [Cap] -> [Cmd]
pleaseConform = map mkCmd . gonyogonyo . group

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

gonyogonyo :: [[Cap]] -> [Range]
gonyogonyo = undefined

