module TypeChecker where
import Lexer
import Parser

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Num _) = Just TNum

typeof ctx (Add e1 e2) = case (typeof e1, typeof e2) of
    (Just TNum, Just TNum) -> Just TNum
    _ -> Nothing

typeof ctx (And e1 e2) = case (typeof e1, typeof e2) of
    (Just TBool, Just TBool) -> Just TBool
    _ -> Nothing

typeof ctx (If e1 e2 e3) =
    case typeof e1 of 
        (Just TBool) -> case (typeof e1, typeof e2) of
            (Just t1, Just t2) -> if t1 == t2 then Just t1 else Nothing
            _ -> Nothing
        _ -> Nothing

-- typeof para operação aritmética de subtração
typeof ctx (Sub e1 e2) = case (typeof e1, typeof e2) of
    (Just TNum, Just TNum) -> Just TNum
    _ -> Nothing

-- typeof para operação aritmética de multiplicação
typeof ctx (Mul e1 e2) = case (typeof e1, typeof e2) of
    (Just TNum, Just TNum) -> Just TNum
    _ -> Nothing

-- typeof para operação lógica de or
typeof ctx (Or e1 e2) = case (typeof e1, typeof e2) of
    (Just TBool, Just TBool) -> Just TBool
    _ -> Nothing

-- typeof para operação lógica de xor
typeof ctx (Xor e1 e2) = case (typeof e1, typeof e2) of
    (Just TBool, Just TBool) -> Just TBool
    _ -> Nothing
