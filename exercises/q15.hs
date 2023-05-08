countList :: [a] -> Int
countList [] = 0
countList l = foldl (\x y -> x + 1) 0 l
