-- Funcao lambda que retorna o triplo de um numero
x :: Int -> Int
x = \x -> x * 3

-- Funcao lamba que verifica se um numero é par
n :: Int -> Bool
n = \n -> if mod n 2 == 0 
                then True 
                else False

-- Funcao lamba que soma dois numeros
somaPar :: Int -> Int -> Int
somaPar = \p q -> p + q

-- Funcao que calcula o dobro de um numero
dobro :: Int -> Int
dobro = \y -> y * 2

-- Funcao que calcula a raiz quadrada de uma lista de inteiros
raiz :: [Int] -> [Float]
raiz = \lst -> map (\x -> sqrt (fromIntegral x)) lst