module TypeChecker where
import Lexer

typeof :: Expr -> Maybe Ty
typeof BTrue = Just TBool
typeof BFalse = Just TBool
typeof (Num _) = Just TNum

typeof (Add e1 e2) = case (typeof e1, typeof e2) of
    (Just TNum, Just TNum) -> Just TNum
    _ -> Nothing

typeof (And e1 e2) = case (typeof e1, typeof e2) of
    (Just TBool, Just TBool) -> Just TBool
    _ -> Nothing

typeof (If e1 e2 e3) =
    case typeof e1 of 
        (Just TBool) -> case (typeof e1, typeof e2) of
            (Just t1, Just t2) -> if t1 == t2 then Just t1 else Nothing
            _ -> Nothing
        _ -> Nothing

-- typeof para operação aritmética de subtração
typeof (Sub e1 e2) = case (typeof e1, typeof e2) of
    (Just TNum, Just TNum) -> Just TNum
    _ -> Nothing

-- typeof para operação aritmética de multiplicação
typeof (Mul e1 e2) = case (typeof e1, typeof e2) of
    (Just TNum, Just TNum) -> Just TNum
    _ -> Nothing

-- typeof para operação lógica de or
typeof (Or e1 e2) = case (typeof e1, typeof e2) of
    (Just TBool, Just TBool) -> Just TBool
    _ -> Nothing

-- typeof para operação lógica de xor
typeof (Xor e1 e2) = case (typeof e1, typeof e2) of
    (Just TBool, Just TBool) -> Just TBool
    _ -> Nothing