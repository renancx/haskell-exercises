-- Metodo utilizando a funcao (^) do Haskell
module Main (main) where

main :: IO()
main = do 
    putStrLn "Digite a base: "
    base <- readLn
    putStrLn "Digite o expoente: "
    potencia <- readLn

    putStr "Resultado: "
    putStrLn (show (base^potencia))

-- Metodo utilizando uma funcao recursiva
potencia :: Int -> Int
potencia 0 = 1
potencia x = 2 * potencia (x-1)