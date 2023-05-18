data Cliente = OrgGov String
            |  Empresa String Int String 
            | Individuo String Bool Pessoa
            deriving Show
    
data Pessoa = Pessoa String Genero
        deriving Show

data Genero = Genero String 
        deriving Show

nomeCliente :: Cliente -> String
nomeCliente cliente = case cliente of 
                        OrgGov nome -> nome
                        Empresa nome _ _ -> nome
                        Individuo nome _ (Pessoa cargo _) -> nome ++ " " ++ cargo


c1 = Individuo "Renan" True (Pessoa "Sigma" (Genero "Homem"))
c2 = OrgGov "UFFS"
c3 = Empresa "Gremio" 10 "Goleiro"

x1 :: String
x1 = case not (1 /= 2) of 
    True -> "Inicio"
    False -> "Fim"

x2 :: String
x2 = case not (1 == 1) of 
    True -> "Inicio"
    False -> "Fim"



pot_dois :: Int -> Int
pot_dois x | x <= 0 = 1
           | otherwise = 2 * pot_dois (x-1)



