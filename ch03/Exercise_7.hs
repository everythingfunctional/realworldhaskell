intersperse :: a -> [[a]] -> [a]
intersperse i [] = []
intersperse i (x:[]) = x
intersperse i (x:xs) = x ++ [i] ++ (intersperse i xs)
