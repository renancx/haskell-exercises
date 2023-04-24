tupla :: (Int, String, Bool)
tupla = (4, "ola", True)

-- ================================

tupla2 :: ((String, Int), Bool)
tupla2 = (("Hello", 4), True)

elemento :: Int
elemento = snd (fst tupla2)

-- ================================

tupla3 :: (Int, String)
tupla3 = (4, "ola")

xx :: Int
xx = fst tupla3

yy :: String
yy = snd tupla3