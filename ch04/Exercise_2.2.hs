import Data.Char (digitToInt)

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "No Number Provided"
asInt_either ('-':xs) = case asInt_either xs of Right number -> Right ((-1) * number)
                                                Left err -> Left err
asInt_either xs
    | elem '.' xs = Left "Decimal Numbers Not supported"
    | otherwise = foldl accumulator (Right 0) xs
        where accumulator (Right acc) x = if elem x ['0'..'9']
                                    then let p = acc * 10 + digitToInt x
                                         in if (acc > p && acc > 0) || (acc < p && acc < 0)
                                                then Left "Integer overflow"
                                                else Right p
                                    else Left ("Invalid non-int char: " ++ [x])
              accumulator (Left err) _ = Left err
