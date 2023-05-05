data Produto =
    Perecivel String String Int Bool Unidade |
    NaoPerecivel String String String Int Peso
    deriving Show

data Unidade = Unidade Int
    deriving Show

data Peso = Peso Int
    deriving Show