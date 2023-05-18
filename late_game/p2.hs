type Matricula = Int
type Nome = String
type Notas = (Double, Double, Double)
type Estudante = (Matricula, Nome, Notas)
type Turma = [Estudante]

exemplo :: Turma
exemplo = [
    ( 123, "Joao", (10, 10, 10) ),
    ( 456, "Maria", (9, 9, 9) ),
    ( 789, "Jose", (2, 2, 2) )
    ]

soma :: (Double, Double, Double) -> Double
soma (x, y, z) = x + y + z

mediaTurma :: Turma -> Double
mediaTurma [] = 0
mediaTurma (h:t) = case h of 
    ( _ , _ , notas) -> (soma notas) / 3 + mediaTurma t

alunosAprovados :: Turma -> [Nome]
alunosAprovados [] = []
alunosAprovados (h:t) = case h of 
    ( _ , nome , notas) -> if (soma notas) / 3 > 6 then nome : alunosAprovados t else alunosAprovados t

media :: Notas -> Double
media (x, y, z) = ((x + y + z) / 3)

type Aluno = (Nome, Notas)

classe :: Turma -> [Aluno]
classe [] = []
classe (h:t) = case h of 
    (_, nome, notas) -> (nome, notas) : classe t
