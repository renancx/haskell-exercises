notas :: Float -> Float -> Float -> String
notas a b c = 
    let media = ((a*2) + (b*3) + (c*5)) / 10
    in
    if media >= 8 && media <= 10 
        then "A"
    else if media > 7 && media <= 8
        then "B"
    else if media > 6 && media <= 7
        then "C"
    else if media > 5 && media <= 6
        then "D"
    else if media >= 0 && media < 5
        then "E"
    else "Valor Invalido"