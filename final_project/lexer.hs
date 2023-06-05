module Lexer where 
import Data.Char

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Or Expr Expr
          | Xor Expr Expr
          | Var String
          | Lam String Expr
          | App Expr Expr
          deriving (Show, Eq)

data Ty = TBool
        | TNum
        deriving (Show, Eq)

data Token = TokenTrue 
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd 
           | TokenSub 
           | TokenMul 
           | TokenAnd 
           | TokenIf 
           | TokenThen 
           | TokenElse 
           | TokenOr 
           | TokenXor 
           deriving (Show, Eq)

isToken :: Char -> Bool
isToken c = elem c "+-*/&|"

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexKW (c:cs)
    | isToken c = lexSymbol (c:cs)
    | otherwise = error ("Lexical error: invalid character " ++ [c])

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of
    (num, rest) -> TokenNum (read num) : lexer rest

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of
                ("true", rest) -> TokenTrue : lexer rest
                ("false", rest) -> TokenFalse : lexer rest
                ("if", rest) -> TokenIf : lexer rest
                ("then", rest) -> TokenThen : lexer rest
                ("else", rest) -> TokenElse : lexer rest
                _ -> error "Lexical error: invalid keyword"

lexSymbol :: String -> [Token]
lexSymbol cs = case span isToken cs of
                ("+", rest) -> TokenAdd : lexer rest
                ("-", rest) -> TokenSub : lexer rest
                ("*", rest) -> TokenMul : lexer rest
                ("&&", rest) -> TokenAnd : lexer rest
                ("||", rest) -> TokenOr : lexer rest
                ("^", rest) -> TokenXor : lexer rest
                _ -> error "Lexical error: invalid symbol"