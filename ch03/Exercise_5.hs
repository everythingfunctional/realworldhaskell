isPalindrome xs
    | len `mod` 2 == 0 = front == reverse back
    | otherwise = front == (reverse (tail back))
    where len = length xs
          (front, back) = splitAt (len `div` 2) xs
