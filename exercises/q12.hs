data Produto =
    Perecivel String String Integer Bool Unidade |
    NaoPerecivel String String String Int Peso
    deriving Show

data Unidade = Unidade Int
    deriving Show

data Peso = Peso Int
    deriving Show

produto :: Produto -> Integer -> Bool
produto (NaoPerecivel _ _ _ _ _) _ = True
produto (Perecivel _ _ x _ _) y = 
    if x > y then True else False

p1 = Perecivel "Leite" "Elegê" 2022 True (Unidade 1)
p2 = NaoPerecivel "Arroz" "Arroz Parabolizado" "PratoFino" 2022 (Peso 10)