digitos :: Int -> [Int]
digitos n | n < 10    = [n]
                        | otherwise = digitos (div n 10) ++ [mod n 10]

obtenerNumero :: [Int] -> Int
obtenerNumero []  = 0
obtenerNumero [x] = x
obtenerNumero (x:xs) = (x * (10 ^ (length(x:xs) - 1))) + obtenerNumero xs

obtenerUltimoElemento :: [Int] -> Int
obtenerUltimoElemento [x]    = x
obtenerUltimoElemento (x:xs) = obtenerUltimoElemento xs

sacarUltimoElemento :: [Int] -> [Int]
sacarUltimoElemento [x] = []
sacarUltimoElemento (x:xs) = x : sacarUltimoElemento xs

sumaDeDigitos :: [Int] -> Int
sumaDeDigitos [x] = x
sumaDeDigitos (x:xs) = x + sumaDeDigitos xs

{- Primer valor: Numero por el cual se quiere saber si es multiplo
   Segundo valor: Numero que se quiere saber si es multiplo -}
esMultiploDe :: Int -> Int -> Bool
esMultiploDe 2  n = esMultiploDe2 (digitos n)
esMultiploDe 3  n = esMultiploDe3 (digitos n)
esMultiploDe 4  n = esMultiploDe4 (digitos n)
esMultiploDe 5  n = esMultiploDe5 (digitos n)
esMultiploDe 6  n = esMultiploDe6 (digitos n)
esMultiploDe 8  n = esMultiploDe8 (digitos n)
esMultiploDe 9  n = esMultiploDe9 (digitos n)
esMultiploDe 10 n = esMultiploDe10 (digitos n)
esMultiploDe 12 n = esMultiploDe12 (digitos n)
esMultiploDe 15 n = esMultiploDe15 (digitos n)

esMultiploDe2 :: [Int] -> Bool
esMultiploDe2 [x]    = (x `mod` 2) == 0
esMultiploDe2 (x:xs) = esMultiploDe2 xs

esMultiploDe3 :: [Int] -> Bool
esMultiploDe3 x | suma < 10 = ((suma == 0) ||
                               (suma == 3) ||
                               (suma == 6) ||
                               (suma == 9))
                | otherwise = esMultiploDe3 (digitos suma)
    where suma = sumaDeDigitos x

esMultiploDe4 :: [Int] -> Bool
esMultiploDe4 [x]    = (x `mod` 4) == 0
esMultiploDe4 [x, y] = ((x * 10 + y) `mod` 4) == 0
esMultiploDe4 (x:xs) = esMultiploDe4 xs

esMultiploDe5 :: [Int] -> Bool
esMultiploDe5 [x]    = (x == 0) || (x == 5)
esMultiploDe5 (x:xs) = esMultiploDe5 xs

esMultiploDe6 :: [Int] -> Bool
esMultiploDe6 x = esMultiploDe2 x && esMultiploDe3 x

esMultiploDe7 :: [Int] -> Bool
esMultiploDe7 x | (abs(cuenta)) < 10 = (abs(cuenta) == 0 || abs(cuenta) == 7)
                | otherwise        = esMultiploDe7 (digitos (abs(cuenta)))
    where cuenta = (obtenerNumero (sacarUltimoElemento x)) -
                                                ((obtenerUltimoElemento x) * 2)

esMultiploDe8 :: [Int] -> Bool
esMultiploDe8 [x]       = (x `mod` 8) == 0
esMultiploDe8 [x, y]    = ((x * 10 + y) `mod` 8) == 0
esMultiploDe8 [x, y, z] = ((x * 100 + y * 10 + z) `mod` 8) == 0
esMultiploDe8 (x:xs)    = esMultiploDe8 xs

esMultiploDe9 :: [Int] -> Bool
esMultiploDe9 x | suma < 10 = (suma == 9)
                | otherwise = esMultiploDe9 (digitos suma)
    where suma = sumaDeDigitos x

esMultiploDe10 :: [Int] -> Bool
esMultiploDe10 [x] = (x == 0)
esMultiploDe10 (x:xs) = esMultiploDe10 xs

{-
esMultiploDe11 :: [Int] -> Bool
esMultiploDe11 [x]       = x == 0
esMultiploDe11 [x, y]    = (x - y) == 0
esMultiploDe11 [x, y, z] = (x + z - y) == 0
esMultiploDe11 (x:y:z)   = esMultiploDe11 z
-}

esMultiploDe12 :: [Int] -> Bool
esMultiploDe12 x = esMultiploDe3 x && esMultiploDe4 x

esMultiploDe15 :: [Int] -> Bool
esMultiploDe15 x = esMultiploDe3 x && esMultiploDe5 x