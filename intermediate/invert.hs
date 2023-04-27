-- Funcao que inverte uma lista de inteiros
reverse :: [Int] -> [Int]
reverse lst = if null lst 
        then []  
        else reverse (tail lst) ++ [head lst]