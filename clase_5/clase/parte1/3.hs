sacar2Elemento :: [a] -> [a]
sacar2Elemento (a:b:c) = a:c -- Funciona a partir de 2 elementos ya que [1,2,3] = (1:(2):[])
sacar2Elemento x       = x