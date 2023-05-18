data FormaPagamento = Dinheiro | Cartao

type Id = Integer
type Produto = String
type Preco = Double
type Quantidade = Integer
type Item = (Produto, Preco, Quantidade)
type Venda = (Id, [Item], FormaPagamento)

exemplo :: Venda 
exemplo = (1, 
            [ ("Bala", 1, 10), 
              ("Pirulito", 2, 5)
            ], 
            Dinheiro
        )

-- Funcao que recebe uma venda e retorna o total de itens 

totalItens :: Venda -> Integer
totalItens (_, itens, _) = sum (map (\(_, _, qtd) -> qtd) itens)

-- Funcao que recebe uma venda e retorna o total da venda

totalVenda :: Venda -> Double
totalVenda (_, itens, _) = sum (map (\(_, preco, qtd) -> preco * fromIntegral qtd) itens)

-- Funcao que retorna apenas os itens cujo valor unitario é maior que 1 real
maioresQueUmReal :: Item -> Bool
maioresQueUmReal (_, preco, _) = case preco of 
                                    preco | preco > 1 -> True
                                    _ -> False

itensCaros :: Venda -> [Item]
itensCaros (_, itens, _) = filter maioresQueUmReal itens

total :: Venda -> [(Produto, Double)]
total (_, itens, _) = map (\(prod, preco, qtd) -> (prod, preco * fromIntegral qtd)) itens