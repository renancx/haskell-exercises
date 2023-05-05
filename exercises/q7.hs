ultimo :: [Int] -> Int
ultimo [] = error "Lista vazia"
ultimo [x] = x
ultimo (x:xs) = ultimo xs