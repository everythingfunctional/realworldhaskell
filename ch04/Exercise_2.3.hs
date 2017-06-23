concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs
