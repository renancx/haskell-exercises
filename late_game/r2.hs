type Matricula = Int
type Nome = String
type Notas = (Double, Double, Double)
type Estudante = (Matricula, Nome, Notas)
type Turma = [Estudante]

exemplo :: Turma
exemplo = [(1234, "Joao", (7.5, 8.0, 9.0)), (5678, "Maria", (6.5, 7.0, 8.0))]

mediaTurma :: Turma -> Double
mediaTurma [] = 0
mediaTurma (h:t) = case h of
    (_, _, (n1, n2, n3)) -> ((n1 + n2 + n3) / 3 + mediaTurma t)

acimaMedia :: Turma -> String
acimaMedia [] = "Turma vazia"
acimaMedia (h:t) = case h of 
                (_, nome, (n01, n02, n03)) -> if (((n01 + n02 + n03)/3) > 6) then nome else acimaMedia t