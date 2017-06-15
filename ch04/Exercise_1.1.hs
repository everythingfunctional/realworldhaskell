safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail [] = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs
safeLast [] = Nothing

safeInit :: [a] -> Maybe [a]
safeInit (x:[]) = Just []
safeInit (x:xs) = Just (x:rest) where (Just rest) = safeInit xs
safeInit [] = Nothing
