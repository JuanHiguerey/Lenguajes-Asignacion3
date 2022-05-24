module Vars(vars) where
import Sintax


--Filtra una lista dado un predicado
filterL p []      = []
filterL p (x:xs)  = if p x then x : filterL p xs else filterL p xs



--elimina repetidos de una lista
nubL []      = []
nubL (x:xs)  = x : (nubL (filterL (\y -> x /= y) xs))      --filtramos usando la funcion lambda (\)



--Devuelve una lista con las variables de la proposicion dada
vars prop =
    nubL (las_vars prop)                --eliminamos los repetidos de la lista resultante
    where
        las_vars prop =
            case prop of
                Constante _              -> []
                Variable var             -> [var]
                Negacion prop1           -> (las_vars prop1)                        --busca la variable de la proposicion del operador
                Conjuncion prop1 prop2   -> (las_vars prop1) ++ (las_vars prop2)    --busca la variable de las proposiciones del operador
                Disyuncion prop1 prop2   -> (las_vars prop1) ++ (las_vars prop2)    --busca la variable de las proposiciones del operador
                Implicacion prop1 prop2  -> (las_vars prop1) ++ (las_vars prop2)    --busca la variable de las proposiciones del operador
                Equivalencia prop1 prop2 -> (las_vars prop1) ++ (las_vars prop2)    --busca la variable de las proposiciones del operador
;