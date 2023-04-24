fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs