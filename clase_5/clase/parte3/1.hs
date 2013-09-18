devolverNImpares :: Int -> [Int]
devolverNImpares 0 = []
devolverNImpares n = devolverNImpares (n - 1) ++ [2 * n - 1]
-- Otra forma: devolver los 2*n numeros impares