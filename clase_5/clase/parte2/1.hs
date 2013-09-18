ordenadaDescendente :: [Int] -> Bool
ordenadaDescendente (a:b:c) | a <= b    = ordenadaDescendente (b:c)
                            | otherwise = False
ordenadaDescendente x                   = True