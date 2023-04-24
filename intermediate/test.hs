tupla :: (Int, String, Bool)
tupla = (4, "ola", True)

-- ================================

tupla2 :: ((String, Int), Bool)
tupla2 = (("Hello", 4), True)

elemento :: Int
elemento = snd (fst tupla2)