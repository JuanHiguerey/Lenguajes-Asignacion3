module EvalProp (evalProp) where
import Sintax 
import As_vals


--busca la variable en un ambiente y devuelve su valor booleano si lo encuentra
busca ident []                          = error ("No esta en el ambiente la variable: " ++ ident)
busca ident ((ident',valor) : ambiente) | ident == ident' = valor
                                        | otherwise       = busca ident ambiente


--Devuelve el valor booleano de la proposicion evaluada en el ambiente dado
evalProp ambiente prop =
    case prop of
        Constante valor         -> valor
        Variable var            -> busca var ambiente
        Negacion prop1          -> not (evalProp ambiente prop1)            --evalua la proposicion del operador
        Conjuncion prop1 prop2  -> (valor1 && valor2) where 
                                    valor1 = (evalProp ambiente prop1)      --evalua las proposiciones del operador
                                    valor2 = (evalProp ambiente prop2)
        Disyuncion prop1 prop2  -> (valor1 || valor2) where 
                                    valor1 = (evalProp ambiente prop1)      --evalua las proposiciones del operador
                                    valor2 = (evalProp ambiente prop2)
        Implicacion prop1 prop2 -> case (valor1, valor2) of 
                                        (True, False) -> False
                                        _             -> True 
                                        where 
                                        valor1 = (evalProp ambiente prop1)   --evalua las proposiciones del operador
                                        valor2 = (evalProp ambiente prop2)
        Equivalencia prop1 prop2 -> valor1 == valor2 where 
                                    valor1 = (evalProp ambiente prop1)       --evalua las proposiciones del operador
                                    valor2 = (evalProp ambiente prop2)
;
