module Taut (taut) where
import Sintax
import As_vals
import EvalProp
import Gen_bools
import Vars


--Evalua si la proposicion es una tautologia
taut prop   | recorrer lista_combinaciones_booleanas = imprimir prop ++ " SI es una tautologia"
            | otherwise = "" 
            where 
                --variables para generar los ambientes
                variables = vars prop
                n = length variables
                lista_combinaciones_booleanas = gen_bools n
                --evaluamos la proposicion en cada ambiente
                recorrer []                 = True
                recorrer (fila : mas_filas)   | evaluacion_es_verdadera = recorrer mas_filas
                                              | otherwise = error (imprimir prop ++ " NO es una tautologia, por el caso " ++ errMSG asociacion) 
                                              where
                                                  --generamos un ambiente para evaluar la proposicion
                                                  asociacion = as_vals variables fila
                                                  evaluacion_es_verdadera = evalProp asociacion prop
                                                  --Especificamos el mensaje de error
                                                  errMSG []          ="solo involucra constantes y la proposicion es falsa"
                                                  errMSG asociacion = impr_as_vals asociacion
;