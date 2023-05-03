precoRetrato :: Integer -> String -> Double
precoRetrato pessoas dia = 
    let 
        preco = if dia == "segunda" || dia == "terca" || dia == "quarta" || dia == "quinta" || dia == "sexta" 
            then 1
        else if dia == "sabado" || dia == "domingo"
            then 120/100
        else 0
    in
        if pessoas == 1
            then 100 * preco
        else if pessoas == 2
            then 130 * preco
        else if pessoas == 3
            then 150 * preco
        else if pessoas == 4
            then 165 * preco
        else if pessoas == 5
            then 175 * preco
        else if pessoas == 6
            then 180 * preco
        else if pessoas >= 7
            then 185 * preco
        else 0