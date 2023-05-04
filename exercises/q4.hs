-- Funcao que calcula o fatoria duplo de um numero
fatorialDuplo :: Int -> Int 
fatorialDuplo 0 = 1
fatorialDuplo 1 = 1
fatorialDuplo n = n * fatorialDuplo (n-2)