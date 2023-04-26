data Nat = Zero 
        | Suc Nat
        deriving Show

-- Numeros de 1 ate 4
um = Suc Zero
dois = Suc (Suc Zero)
tres = Suc (Suc (Suc Zero))
quatro = Suc (Suc (Suc (Suc Zero)))

-- Converter Nat para Integer
nat2integer :: Nat -> Integer
nat2integer Zero = 0
nat2integer (Suc n) = 1 + nat2integer n

-- Converter Integer para Nat
integer2nat :: Integer -> Nat
integer2nat 0 = Zero
integer2nat n = Suc (integer2nat (n-1))

-- Somar dois números naturais
natAdd :: Nat -> Nat -> Nat
natAdd Zero n = n
natAdd (Suc m) n = Suc (natAdd m n)

-- Subtrair dois números naturais
natSub :: Nat -> Nat -> Nat
natSub Zero _ = Zero
natSub m Zero = m
natSub (Suc m) (Suc n) = natSub m n

-- Multiplicar dois números naturais
natMul :: Nat -> Nat -> Nat
natMul Zero _ = Zero
natMul (Suc m) n = natAdd n (natMul m n)