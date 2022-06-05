module Bonita (Proposicion (..)) where 
import Sintax
import FuncAux

--Recibe dos proposiciones. Retorna True si izq tiene mayor precedencia que der, False en caso contrario
es_prio (izq, der) =
    --variables y constantes se consideran siempre con mayor precedencia para que nunca esten entre parentesis
    if esVariable izq || esConstante izq then
        True
    --La negacion tiene la precedencia mas grande 
    else if esNegacion izq then
        True
    --Seguido de Conjuncion 
    else if esConjuncion izq then
        if esNegacion der then
            False
        else
            True
    --Luego Disyuncion
    else if esDisyuncion izq then
        if esNegacion der || esConjuncion der then
            False
        else
            True
    --Implicacion 
    else if esImplicacion izq then
        if esNegacion der || esConjuncion der || esDisyuncion der then
            False
        else
            True
    --De ultimo esta la Equivalencia
    else
        if esNegacion der || esConjuncion der || esDisyuncion der || esImplicacion der then
            False
        else
            True
;

bonita prop =
    let
        forma_tipo_proposicion = imprimir prop  --Se obtiene el string en notacion tipo Proposicion
        --interno desconstruye la proposicion recursivamente. La convierte en string, insertando parentesis donde sea necesario 
        interno prop =
            case prop of
                --Las constantes se imprimen tal cual
                Constante valor -> imprimir prop
                --Las variables se imprimen tal cual 
                Variable var    ->  var
                --Las variables, constantes, y negaciones jamas van en parentesis cuando la proposicion externa es una negacion. Los demas casos siempre van en parentesis
                Negacion p1
                    ->  if not(es_prio(p1, Negacion p1)) then
                            "¬ " ++ "(" ++ interno p1 ++ ")"
                        else
                            "¬ " ++ interno p1
                {- Los casos de Conjuncion, Disyuncion, Implicacion, y Equivalencia tienen exactamente la misma logica 
                 La idea es poner entre parentesis solo a las proposiciones internas que tienen menor precedencia que la proposicion externa 
                 Si una proposicion 1 esta adentro de proposicion 2 y proposicion 1 tiene menor precedencia, entonces proposicion 1 debe esta entre parentesis -}
                Conjuncion p1 p2
                        -- Ambas proposiciones internas tienen mayor precedencia entonces no se incluye parentesis 
                    ->  if es_prio(p1, Conjuncion p1 p2) && es_prio(p2, Conjuncion p1 p2) then
                            interno p1 ++ " /\\ " ++ interno p2
                        --Solo se pone entre parentesis a las proposiciones internas con menor precedencia que la proposicion externa
                        else if es_prio(p1, Conjuncion p1 p2) && not(es_prio(p2, Conjuncion p1 p2)) then
                            interno p1 ++ " /\\ " ++ "(" ++ interno p2 ++ ")"
                        --Solo se pone entre parentesis a las proposiciones internas con menor precedencia que la proposicion externa
                        else if not(es_prio(p1, Conjuncion p1 p2)) && es_prio(p2, Conjuncion p1 p2) then
                            "(" ++ interno p1 ++ ")" ++ " /\\ " ++ interno p2
                        --Ambas proposiciones internas tienen menor precedencia, entonces ambas van entre parentesis
                        else
                            "(" ++ interno p1 ++ ")" ++ " /\\ " ++ "(" ++ interno p2 ++ ")"
                --El mismo procedimiento aplica para los demas casos
                Disyuncion p1 p2
                    ->  if es_prio(p1, Disyuncion p1 p2) && es_prio(p2, Disyuncion p1 p2) then
                            interno p1 ++ " \\/ " ++ interno p2
                        else if es_prio(p1, Disyuncion p1 p2) && not(es_prio(p2, Disyuncion p1 p2)) then
                            interno p1 ++ " \\/ " ++ "(" ++ interno p2 ++ ")"
                        else if not(es_prio(p1, Disyuncion p1 p2)) && es_prio(p2, Disyuncion p1 p2) then
                            "(" ++ interno p1 ++ ")" ++ " \\/ " ++ interno p2
                        else
                            "(" ++ interno p1 ++ ")" ++ " \\/ " ++ "(" ++ interno p2 ++ ")"
                Implicacion p1 p2
                    ->  if es_prio(p1, Implicacion p1 p2) && es_prio(p2, Implicacion p1 p2) then
                            interno p1 ++ " => " ++ interno p2
                        else if es_prio(p1, Implicacion p1 p2) && not(es_prio(p2, Implicacion p1 p2)) then
                            interno p1 ++ " => " ++ "(" ++ interno p2 ++ ")"
                        else if not(es_prio(p1, Implicacion p1 p2)) && es_prio(p2, Implicacion p1 p2) then
                            "(" ++ interno p1 ++ ")" ++ " => " ++ interno p2
                        else
                            "(" ++ interno p1 ++ ")" ++ " => " ++ "(" ++ interno p2 ++ ")"
                Equivalencia p1 p2
                    ->  if es_prio(p1, Equivalencia p1 p2) && es_prio(p2, Equivalencia p1 p2) then
                            interno p1 ++ " <=> " ++ interno p2
                        else if es_prio(p1, Equivalencia p1 p2) && not(es_prio(p2, Equivalencia p1 p2)) then
                            interno p1 ++ " <=> " ++ "(" ++ interno p2 ++ ")"
                        else if not(es_prio(p1, Equivalencia p1 p2)) && es_prio(p2, Equivalencia p1 p2) then
                            "(" ++ interno p1 ++ ")" ++ " <=> " ++ interno p2
                        else
                            "(" ++ interno p1 ++ ")" ++ " <=> " ++ "(" ++ interno p2 ++ ")"
            in
                --Primero se imprime el string en formato Proposicion y luego el string en formato bonito
                putStr(forma_tipo_proposicion ++ "\n" ++ interno prop ++ "\n\n")
;




test1 :: Proposicion
test1 = (vp .=> (vq \/ vr)) <=> ((¬)vp /\ vr);     --Ejemplo 37 Murillo
test2 :: Proposicion
test2 = ((¬)vp /\ vq) .=> (vq .=> vp);                 --Ejemplo 35 Murillo
test3 :: Proposicion
test3 = (vp .=> vq) /\ vr;                                   --Ejemplo 1.b Murillo