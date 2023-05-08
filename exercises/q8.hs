--Funcao que seleciona todos os elementos de uma lista vazia, exceto o ultimo
primeiros :: [Int] -> [Int]
primeiros [] = error "Lista vazia"
primeiros [x] = []
primeiros (x:xs) = x : primeiros xs 

--Usando reverse
first :: [Int] -> [Int]
first l = reverse (tail (reverse l))