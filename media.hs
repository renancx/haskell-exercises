module Main (main) where

main :: IO()
main = do 
    putStrLn "Digite um número: "
    n1 <- readLn
    putStrLn "Digite outro número: "
    n2 <- readLn
    putStrLn "Digite mais um número: "
    n3 <- readLn

    putStr "Média dos números digitados: "
    putStrLn (show ((n1+n2+n3)/3))