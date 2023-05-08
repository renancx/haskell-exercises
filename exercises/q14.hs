calcLista :: Num a => [a] -> a
calcLista [] = 0
calcLista [x] = x
calcLista (x:xs) = x + calcLista xs