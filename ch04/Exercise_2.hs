splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith predicate xs = first:(splitWith predicate rest)
    where (first, remaining) = break predicate xs
          rest = if (null remaining) then [] else dropWhile predicate remaining
