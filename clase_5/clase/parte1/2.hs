estaEnLaLista :: [Int] -> Int -> Bool
estaEnLaLista [] x                 = False
estaEnLaLista (j:js) x | j == x    = True
					   | otherwise = estaEnLaLista js x