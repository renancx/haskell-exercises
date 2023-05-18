data FormaPagamento = Dinheiro | Cartao

type Id = Integer
type Produto = String
type Preco = Double
type Quantidade = Integer
type Item = (Produto, Preco, Quantidade)
type Venda = (Id, [Item], FormaPagamento)

exemplo :: Venda 
exemplo = (1, 
            [ ("Bala", 0.5, 10), 
              ("Pirulito", 0.25, 5)
            ], 
            Dinheiro
        )

-- Funcao que recebe uma venda e retorna o total de itens 

totalItens :: Venda -> Integer
totalItens (_, itens, _) = sum (map (\(_, _, qtd) -> qtd) itens)

-- Funcao que recebe uma venda e retorna o total da venda

totalVenda :: Venda -> Double
totalVenda (_, itens, _) = sum (map (\(_, preco, qtd) -> preco * fromIntegral qtd) itens)

