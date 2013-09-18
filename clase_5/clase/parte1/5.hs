esPalindroma :: [Int] -> Bool
esPalindroma (x:xs) = ((x:xs) == invertir (x:xs))

invertir :: [Int] -> [Int]
invertir []    = []
invertir (x:xs) = invertir xs ++ [x]