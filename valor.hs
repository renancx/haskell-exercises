module Salario (salario) where

salario :: IO()
salario = do 
    putStrLn "Digite o salario minimo: "
    n1 <- readLn
    putStrLn "Digite a quantidade de kWatts: "
    n2 <- readLn

    let vkWatts = n1 / 5
    let n3 = n2 * vkWatts

    putStr "Valor a ser pago: " --com 15% de desconto
    putStrLn (show (n3 * (0.85)))