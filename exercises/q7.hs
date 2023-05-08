ultimo :: [Int] -> Int
ultimo [] = error "Lista vazia"
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- Usando outro metodo
last :: [Int] -> Int
last l = head (reverse l)