module FuncNorAux where 
    --Funciones auxiliares para obtener las simplificaciones de la expresión dada.

--Modulos necesarios
import Sintax

-- Crea la expresión de forma negativa a la dada. La cual va llamando a las demas funciones
-- para simplificar la expresión dada.
obtenerNegaciones :: Proposicion -> Proposicion
obtenerNegaciones f = negarFunct (borrarImpl (borrarEquiv f))

--Borra todas las equivalencias dentro de la función dada. Ejemplos: 
--                                           (vp <=> vq)   -->  ((vp .=> vq) /\ (vq .=> vp))
borrarEquiv :: Proposicion -> Proposicion
borrarEquiv (Variable f)        = (Variable f)
borrarEquiv (Negacion f)        = Negacion (borrarEquiv f) 
borrarEquiv (Conjuncion f g)    = Conjuncion (borrarEquiv f) (borrarEquiv g) 
borrarEquiv (Disyuncion f g)    = Disyuncion (borrarEquiv f) (borrarEquiv g) 
borrarEquiv (Implicacion f g)   = Implicacion (borrarEquiv f) (borrarEquiv g) 
borrarEquiv (Equivalencia f g)  =
    Conjuncion (Implicacion (borrarEquiv f) (borrarEquiv g))
    (Implicacion (borrarEquiv g) (borrarEquiv f))

--Borra todas las implicaciones que existen en la expresión dada. Ejemplos: 
--                                           (vp .=> vq)       -->    (¬vp \/ vq)
borrarImpl :: Proposicion -> Proposicion
borrarImpl (Variable f)       = (Variable f)
borrarImpl (Negacion f)       = Negacion (borrarImpl f) 
borrarImpl (Conjuncion f g)   = Conjuncion (borrarImpl f) (borrarImpl g) 
borrarImpl (Disyuncion f g)   = Disyuncion (borrarImpl f) (borrarImpl g) 
borrarImpl (Implicacion f g)  = Disyuncion (Negacion (borrarImpl f)) (borrarImpl g) 

-- Aplica las negaciones invocradas en la expresión, ejemplos:
--                                           (¬(¬vp))       -->     (vp)
--                                           (¬(vp /\ vq))  -->     (¬vp \/ ¬vq)
negarFunct :: Proposicion -> Proposicion
negarFunct (Variable f)        = (Variable f)
negarFunct (Negacion f)        = negarFunctAux f
negarFunct (Conjuncion f g)    = Conjuncion (negarFunct f) (negarFunct g) 
negarFunct (Disyuncion f g)    = Disyuncion (negarFunct f) (negarFunct g) 

-- Funcion auxiliar de las negaciones cuando se encuentra una doble negación. Ejemplo:
--                                           (¬(¬ (vp \/ vq)))  -->  (vp \/ vq)
negarFunctAux :: Proposicion -> Proposicion
negarFunctAux (Variable f)     = Negacion (Variable f)
negarFunctAux (Negacion f)     = negarFunct f 
negarFunctAux (Conjuncion f g) = Disyuncion (negarFunctAux f) (negarFunctAux g) 
negarFunctAux (Disyuncion f g) = Conjuncion (negarFunctAux f) (negarFunctAux g) 


{- --Ejemplos de formas normales disyuntivas
let test1 = fnd (vp /\ (vq .=> vr))             
let test2 = fnd ((vp <=> vq) .=> vr)               

let test3 = fnc ((¬)((vp \/ vq) .=> (vp /\ vq)))   
let test4 = fnc (vp /\ (vq .=> vr))                

let res = imprimir test1

do {
    --print res 
    --putStr "FND: \n" ; print test1 ;
    putStr "\nfnc (vp /\\ (vq .=> vr))   : \n" ; print res ;
    putStr "\n\n";
} -}