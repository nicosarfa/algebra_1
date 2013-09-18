esCapicua :: [Int] -> Bool
esCapicua (x:xs) = (x:xs) == reverse (x:xs)

esCapicua2 :: [Int] -> Bool
esCapicua2 (x:xs) = ((x:xs) == invertirLista (x:xs))

invertirLista :: [Int] -> [Int]
invertirLista [a]    = [a]
invertirLista (x:xs) = invertirLista (xs) ++ [x]
