sacarNegativos :: [Int] -> [Int]
sacarNegativos []                 = []
sacarNegativos (x:xs) | x < 0     = sacarNegativos xs
                      | otherwise = x: sacarNegativos xs