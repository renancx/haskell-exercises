countList :: [a] -> Int
countList [] = 0
countList l = foldl (\x y -> x + 1) 0 l

-- filter, fold e map
-- usado para manipular lista

primeiro :: [Integer] -> Integer
primeiro f = head f

ultimo :: [Integer] -> Integer
ultimo u = last u

list = [1, 5, 3, 2, 4]

pares :: [Integer] -> [Integer]
pares [] = []
pares b = (filter (not . odd) b)