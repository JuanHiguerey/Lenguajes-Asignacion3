module Fnc where

--Modulos necesarios
import Sintax
import FuncNorAux

--Funcion para obtener la forma normal conjuntiva.
--  Primero simplifica la expresión y luego realiza las disyunciones respectivas. 
fnc :: Proposicion -> Proposicion
fnc f = crearDisyunciones (obtenerNegaciones f)

--Una vez la expresión simplificada se crean las disyunciones respectivas de la expresion simplificada.
crearDisyunciones :: Proposicion -> Proposicion
crearDisyunciones (Disyuncion (Conjuncion f1 f2) g) = 
    crearDisyunciones 
    (Conjuncion (Disyuncion (crearDisyunciones f1) (crearDisyunciones g))
                (Disyuncion (crearDisyunciones f2) (crearDisyunciones g)))
crearDisyunciones (Disyuncion f (Conjuncion g1 g2)) =
    crearDisyunciones
    (Conjuncion (Disyuncion (crearDisyunciones f) (crearDisyunciones g1))
    (Disyuncion (crearDisyunciones f) (crearDisyunciones g2)))
crearDisyunciones (Conjuncion f g) = 
    Conjuncion (crearDisyunciones f) (crearDisyunciones g)
crearDisyunciones f = f