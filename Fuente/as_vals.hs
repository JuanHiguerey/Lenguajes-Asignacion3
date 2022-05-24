module As_vals (as_vals, impr_as_vals) where


--Creamos una lista juntando cada elemento de una lista con el 
--elemento del mismo indice de otra lista de igual tamano
zipVB []        []        = []
zipVB (x : xs)  (y : ys)  = (x, y) : zipVB xs ys        --juntamos los elementos de las listas
zipVB []        _         = []                          --listas de diferente tamano
zipVB _         _         = []                          --listas de diferente tamano



--Devuelve una lista con cada variable de vars con su valor de bools
--vars y bools deben ser del mismo tamano
as_vals vars bools = zipVB vars bools



--Devuelve un string mostrando cada variable con su valor booleano de una lista
impr_as_vals []             = ""
impr_as_vals ((v,b) : vbs) = "(" ++ v ++ "," ++ (if b then "true" else "false") ++ ") " ++ impr_as_vals  vbs
