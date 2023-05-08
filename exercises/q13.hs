and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True