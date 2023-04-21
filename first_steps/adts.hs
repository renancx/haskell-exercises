module Main where

data Cliente = OrgGov String
            | Empresa String Integer String String
            | Individuo Pessoa
            deriving Show

data Pessoa = Pessoa String String Genero
            deriving Show

data Genero = Masculino | Feminino | Outro String
            deriving Show

nomeCliente :: Cliente -> String
nomeCliente cliente = case cliente of
                        OrgGov nome                 -> nome
                        Empresa nome id resp cargo  -> nome
                        Individuo pessoa -> 
                            case pessoa of 
                                Pessoa pNome sNome g -> pNome ++ " " ++ sNome

main :: IO ()
main = putStrLn "Oi"