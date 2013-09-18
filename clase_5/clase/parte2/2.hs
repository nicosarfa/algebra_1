estaEnLaLista :: [Int] -> Int -> Bool
estaEnLaLista [] x                 = False
estaEnLaLista (j:js) x | j == x    = True
					   | otherwise = estaEnLaLista js x

hayRepetidos :: [Int] -> Bool
hayRepetidos (x:xs) | estaEnLaLista xs x = True
                    | otherwise          = hayRepetidos xs
hayRepetidos []                          = False