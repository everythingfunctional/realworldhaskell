import Data.List

sortByLength xs = sortBy (\x y -> (length x) `compare` (length y)) xs
