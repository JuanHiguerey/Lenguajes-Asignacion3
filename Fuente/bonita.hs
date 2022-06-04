module Bonita (Proposicion (..)) where 
import Sintax
import FuncAux

--Recibe dos proposiciones. Retorna True si izq tiene mayor precedencia que der, False en caso contrario
es_prio (izq, der) =
    --variables y constantes se consideran siempre con mayor precedencia para que nunca esten entre parentesis
    if es_variable(izq) || es_constante(izq) then
        True
    --La negacion tiene la precedencia mas grande 
    else if es_negacion(izq) then
        True
    --Seguido de conjuncion 
    else if es_conjuncion(izq) then
        if es_negacion(der) then
            False
        else
            True
    --Luego disyuncion
    else if es_disyuncion(izq) then
        if es_negacion(der) || es_conjuncion(der) then
            False
        else
            True
    --Implicacion 
    else if es_implicacion(izq) then
        if es_negacion(der) || es_conjuncion(der) || es_disyuncion(der) then
            False
        else
            True
    --De ultimo esta la equivalencia
    else
        if es_negacion(der) || es_conjuncion(der) || es_disyuncion(der) || es_implicacion(der) then
            False
        else
            True
;



test1 = (vp (.=>) (vq (\/) vr)) (<=>) ((¬)(vp (/\) vr));     --Ejemplo 37 Murillo
test2 = ((¬)(vp (/\) vq)) (.=>) (vq (.=>) vp);             --Ejemplo 35 Murillo
test3 = (vp (.=>) vq) (/\) vr;                           --Ejemplo 1.b Murillo