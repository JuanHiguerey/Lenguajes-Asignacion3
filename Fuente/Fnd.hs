module Fnd where

--Modulos necesarios
import Sintax
import FuncNorAux

--Funcion para obtener la forma normal disyuntiva.
--  Primero simplifica la expresión y luego realiza las conjuciones respectivas. 
fnd :: Proposicion -> Proposicion
fnd f = crearConjuncion (obtenerNegaciones f)

--Una vez la expresión simplificada se crean las conjunciones respectivas de la expresion simplificada.
crearConjuncion :: Proposicion -> Proposicion
crearConjuncion (Conjuncion (Disyuncion f1 f2) g) =
    crearConjuncion
    (Disyuncion (Conjuncion (crearConjuncion f1) (crearConjuncion g))
                (Conjuncion (crearConjuncion f2) (crearConjuncion g)))
crearConjuncion (Conjuncion f (Disyuncion g1 g2)) =
    crearConjuncion
    (Disyuncion (Conjuncion (crearConjuncion f) (crearConjuncion g1))
                (Conjuncion (crearConjuncion f) (crearConjuncion g2)))
crearConjuncion (Disyuncion f g) =  
    Disyuncion (crearConjuncion f) (crearConjuncion g)
crearConjuncion f = f