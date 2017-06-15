import Data.Char (digitToInt)

asInt_fold :: String -> Int
asInt_fold "" = error "No Number Provided"
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold xs
    | elem '.' xs = error "Decimal Numbers Not supported"
    | otherwise = foldl accumulator 0 xs
        where accumulator acc x = if elem x ['0'..'9']
                                    then let p = acc * 10 + digitToInt x
                                         in if (acc > p && acc > 0) || (acc < p && acc < 0)
                                                then error "Integer overflow"
                                                else p
                                    else error ("Invalid non-int char: " ++ [x])
