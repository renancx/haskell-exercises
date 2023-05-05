produtoListas :: [Int] -> [Int] -> [Int] 
produtoListas [] [] = []
produtoListas (x:xs) (y:ys) = (x*y) : produtoListas xs ys