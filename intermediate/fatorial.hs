fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

--funcao que calcula o tamanho de uma lista
length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

--funcao que calcula a potencia de dois
potDois :: Integer -> Integer
potDois n 
    | n == 0 = 1
    | otherwise = 2 * potDois (n-1)