module Gen_bools (gen_bools) where


--Devuelve una lista con todas las posibles combinaciones de booleanos de n variables
gen_bools 0 = [[]]
gen_bools n = (map (True :) anterior) ++ (map (False :) anterior)
              where anterior = gen_bools (n - 1)