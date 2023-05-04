salario :: Double -> Double -> Double -> Double 
salario inicial anoContracao anoAtual =
    if anoAtual - anoContracao == 0 then inicial 
    else salario (inicial * 1.015) (anoContracao + 1) anoAtual