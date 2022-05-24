module Tabla_verdad (tabla_dato, tabla, tabla_str) where
import Sintax
import As_vals
import EvalProp
import Gen_bools
import Vars


--Devuelve una lista con cada ambiente y el resultado de evaluar la proposicion en ese ambiente
tabla_dato prop = recorrer lista_combinaciones_booleanas
                  where 
                      --variables para generar los ambientes
                      variables = vars prop
                      n = length variables
                      lista_combinaciones_booleanas = gen_bools n
                      --Creamos una lista con cada ambiente y el resultado de evaluar la proposicion en ese ambiente
                      recorrer []                 = []
                      recorrer (fila : mas_filas) = (asociacion, resultado_fila) : recorrer mas_filas
                                                    where
                                                        --generamos un ambiente para evaluar la proposicion
                                                        asociacion = as_vals variables fila
                                                        resultado_fila = evalProp asociacion prop
;



--Imprime cada ambiente y el resultado de evaluar la proposicion en ese ambiente
tabla prop = putStr(recorrer lista_combinaciones_booleanas)
             where 
                 --variables para generar los ambientes
                 variables = vars prop
                 n = length variables
                 lista_combinaciones_booleanas = gen_bools n
                 --funcion que devuelve un string mostrando el ambiente y el resultado de evaluar la proposicion en el ambiente
                 imprimir_fila vars_bools es_verdadero = impr_as_vals vars_bools ++ " === " ++ (if es_verdadero then "true" else "false") ++ "\n"
                 --Creamos un string con cada ambiente y el resultado de evaluar la proposicion en ese ambiente
                 recorrer []                 = "\n"
                 recorrer (fila : mas_filas) = imprimir_fila  asociacion  resultado_fila ++ recorrer mas_filas
                                               where
                                                   --generamos un ambiente para evaluar la proposicion
                                                   asociacion = as_vals variables fila
                                                   resultado_fila = evalProp asociacion prop
;



--Devuelve un string mostrando cada ambiente y el resultado de evaluar la proposicion en ese ambiente
tabla_str prop = recorrer lista_combinaciones_booleanas
                 where 
                     --variables para generar los ambientes
                     variables = vars prop
                     n = length variables
                     lista_combinaciones_booleanas = gen_bools n
                     --funcion que devuelve el string mostrando el ambiente y el resultado de evaluar la proposicion en ese ambiente
                     mostrar_fila vars_bools es_verdadero = impr_as_vals vars_bools ++ " === " ++ (if es_verdadero then "true" else "false") ++ "\n"
                     --Creamos un string con cada ambiente y el resultado de evaluar la proposicion en ese ambiente
                     recorrer []                 = ""
                     recorrer (fila : mas_filas) = mostrar_fila  asociacion  resultado_fila ++ recorrer mas_filas
                                                   where
                                                       --generamos un ambiente para evaluar la proposicion
                                                       asociacion = as_vals variables fila
                                                       resultado_fila = evalProp asociacion prop
;
