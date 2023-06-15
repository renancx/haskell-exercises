module Interpreter where
import Lexer
import Parser

isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse = True
isValue (Num _) = True
isValue _ = False

subst :: String -> Expr -> Expr -> Expr
subst x n BTrue = BTrue
subst x n BFalse = BFalse
subst x n (Num n) = Num n
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (Mul e1 e2) = Mul (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (If e1 e2 e3) = If (subst x n e1) (subst x n e2) (subst x n e3)
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
subst x n (Xor e1 e2) = Xor (subst x n e1) (subst x n e2)
subst x n (Var v) | x == y = n
                  | otherwise = Var v
subst x n (Lam v t e) = Lam v t (subst x n e)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)

step :: Expr -> Expr
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n) e2) = Add (Num n) (step e2)
step (Add e1 e2) = Add (step e1) e2

step (And BFalse e2) = BFalse
step (And BTrue e2) = e2
step (And e1 e2) = And (step e1) e2

step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e1 e2 e3) = If (step e1) e2 e3

-- step para a operação aritmética de subtração
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n) e2) = Sub (Num n) (step e2)
step (Sub e1 e2) = Sub (step e1) e2

-- step para a operação aritmética de multiplicação
step (Mul (Num n1) (Num n2)) = Num (n1 * n2)
step (Mul (Num n) e2) = Mul (Num n) (step e2)
step (Mul e1 e2) = Mul (step e1) e2

-- step para a operação lógica de or
step (Or BFalse e2) = e2
step (Or BTrue e2) = BTrue
step (Or e1 e2) = Or (step e1) e2

-- step para operação lógica de xor
step (Xor BFalse BFalse) = BFalse
step (Xor BTrue BTrue) = BFalse
step (Xor BFalse BTrue) = BTrue
step (Xor BTrue BFalse) = BTrue
step (Xor e1 e2) = Xor (step e1) e2

step (App (Lam x t b) e) | isValue e = subst x e b
                       | otherwise = App (Lam x t b) (step e)
step (App e1 e2) = App (step e1) e2

eval :: Expr -> Expr
eval BTrue = BTrue
eval BFalse = BFalse
eval (Num x) = Num x
eval e' = eval (step e')

typecheck :: Expr -> Expr
typecheck e = case typeof [] e of
    Just _ -> e
    Nothing -> error "Type error"