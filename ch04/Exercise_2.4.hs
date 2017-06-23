myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs)
    | f x = x : myTakeWhile f xs
    | otherwise = []

myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' f xs = foldr next [] xs
    where next x ys
            | f x = x : ys
            | otherwise = []
