module Simpl (absorcion, idempotencia, neutro, simpl) where
import Sintax
import FuncAux

-- Funcion que recibe una proposicion e intenta simplificarla mediante absorcion. Si no es posible simplificar, se retorna la misma proposicion de entrada
-- Se evalua los casos: Disyuncion externa, Conjuncion externa, y las mismas con negaciones para un total de 16 reglas de simplificacion
absorcion :: Proposicion -> Proposicion
absorcion (Disyuncion prop1 prop2) =
    if (esConjuncion prop2) && (prop1 == getProp1 prop2) then prop1         -- P | (P & Q)
    else if (esConjuncion prop2) && (prop1 == getProp2 prop2) then prop1    -- P | (Q & P)
    else if (esConjuncion prop1) && (prop2 == getProp1 prop1) then prop2    -- (P & Q) | P
    else if (esConjuncion prop1) && (prop2 == getProp2 prop1) then prop2    -- (Q & P) | P
    else if (esConjuncion prop2 && esNegacion (getProp1 prop2)) && (prop1 == getProp1 (getProp1 prop2)) then Disyuncion prop1 (getProp2 prop2)     -- P | (¬P & Q)
    else if (esConjuncion prop2 && esNegacion (getProp1 prop2)) && (prop1 == getProp1 (getProp2 prop2)) then Disyuncion prop1 (getProp1 prop2)     -- P | (Q & ¬P)
    else if (esConjuncion prop1 && esNegacion (getProp1 prop1)) && (prop2 == getProp1 (getProp1 prop1)) then Disyuncion prop2 (getProp2 prop1)     -- (¬P & Q) | P
    else if (esConjuncion prop1 && esNegacion (getProp2 prop1)) && (prop2 == getProp1 (getProp2 prop1)) then Disyuncion prop2 (getProp1 prop2)     -- (Q & ¬P) | P
    else Disyuncion prop1 prop2                                             -- Cualquier otro caso, se retorna la entrada tal cual
absorcion (Conjuncion prop1 prop2) =   
    if (esDisyuncion prop2) && (prop1 == getProp1 prop2) then prop1         -- P & (P | Q)
    else if (esDisyuncion prop2) && (prop1 == getProp2 prop2) then prop1    -- P & (Q | P)
    else if (esDisyuncion prop1) && (prop2 == getProp1 prop1) then prop2    -- (P | Q) & P
    else if (esDisyuncion prop1) && (prop2 == getProp2 prop1) then prop2    -- (Q | P) & P
    else if (esDisyuncion prop2 && esNegacion (getProp1 prop2)) && (prop1 == getProp1 (getProp1 prop2)) then Conjuncion prop1 (getProp2 prop2)      -- P & (¬P | Q)
    else if (esDisyuncion prop2 && esNegacion (getProp1 prop2)) && (prop1 == getProp1 (getProp2 prop2)) then Conjuncion prop1 (getProp1 prop2)      -- P & (Q | ¬P)
    else if (esDisyuncion prop1 && esNegacion (getProp1 prop1)) && (prop2 == getProp1 (getProp1 prop1)) then Conjuncion prop2 (getProp2 prop1)      -- (¬P | Q) & P
    else if (esDisyuncion prop1 && esNegacion (getProp2 prop1)) && (prop2 == getProp1 (getProp2 prop1)) then Conjuncion prop2 (getProp1 prop2)      -- (Q | ¬P) & P
    else Conjuncion prop1 prop2                                             -- Cualquier otro caso, se retorna la entrada tal cual
absorcion (Constante valor) = Constante valor
absorcion (Variable nombre) = Variable nombre                               
absorcion (Negacion prop) = Negacion prop
absorcion (Implicacion prop1 prop2) = Implicacion prop1 prop2
absorcion (Equivalencia prop1 prop2) = Equivalencia prop1 prop2

-- Funcion que recibe una proposicion e intenta simplificarla mediante idempotencia. Si no es posible simplificar, se retorna la misma proposicion de entrada
-- Se evalua los casos: Disyuncion con las mismas proposiciones internas, Conjuncion con las mismas proposiciones internas para un total de 2 reglas de simplificacion
idempotencia :: Proposicion -> Proposicion
idempotencia (Conjuncion prop1 prop2) =
    if (prop1 == prop2) then prop1    -- P & P
    else Conjuncion prop1 prop2
idempotencia (Disyuncion prop1 prop2) =
    if (prop1 == prop2) then prop1    -- P | P
    else Disyuncion prop1 prop2                         -- Cualquier otro caso se retorna la entrada tal cual
idempotencia (Constante valor) = Constante valor
idempotencia (Variable nombre) = Variable nombre                               
idempotencia (Negacion prop) = Negacion prop
idempotencia (Implicacion prop1 prop2) = Implicacion prop1 prop2
idempotencia (Equivalencia prop1 prop2) = Equivalencia prop1 prop2

-- Funcion que recibe una proposicion e intenta simplificarla mediante neutro. Si no es posible simplificar, se retorna la misma proposicion de entrada
-- Se evalua los casos: Conjuncion con un True y Disyuncion con un False para un total de 4 reglas de simplificacion
neutro :: Proposicion -> Proposicion
neutro (Conjuncion prop1 (Constante valor)) =
    if not(esConstante(prop1)) && valor then prop1         -- P & True
    else Conjuncion prop1 (Constante valor)
neutro (Conjuncion (Constante valor) prop2) =
    if not(esConstante(prop2)) && valor then prop2         -- True & P
    else Conjuncion (Constante valor) prop2
neutro (Disyuncion prop1 (Constante valor)) =
    if not(esConstante(prop1)) && not(valor) then prop1    -- P | False
    else Disyuncion prop1 (Constante valor)
neutro (Disyuncion (Constante valor) prop2) =
    if not(esConstante(prop2)) && not(valor) then prop2    -- False | P
    else Disyuncion (Constante valor) prop2
neutro (Constante valor) = Constante valor
neutro (Variable nombre) = Variable nombre                               
neutro (Negacion prop) = Negacion prop
neutro (Conjuncion prop1 prop2) = Conjuncion prop1 prop2
neutro (Disyuncion prop1 prop2) = Disyuncion prop1 prop2
neutro (Implicacion prop1 prop2) = Implicacion prop1 prop2
neutro (Equivalencia prop1 prop2) = Equivalencia prop1 prop2


-- Funcion que recibe una proposicion e intenta simplificarla utilizando leyes logicas. Incluye simplificaciones por absorcion, idempotencia, y neutro.
simpl :: Proposicion -> Proposicion
simpl (Constante valor) = Constante valor   -- Las constantes se retornan tal cual
simpl (Variable nombre) = Variable nombre   -- Las variables se retornan tal cual
simpl (Negacion prop) = Negacion (simpl prop)   -- Se intenta simplificar la proposicion interna de la negacion
simpl (Conjuncion prop1 prop2) =
    let
        -- primero se aplican las simplificacions a la proposicion global y a sus componentes
        abs = (absorcion (Conjuncion prop1 prop2))
        idem = (idempotencia (Conjuncion prop1 prop2))
        neu = (neutro (Conjuncion prop1 prop2))
        abs1 = (absorcion prop1)
        idem1 = (idempotencia prop1)
        neu1 = (neutro prop1)
        abs2 = (absorcion prop2)
        idem2 = (idempotencia prop2)
        neu2 = (neutro prop2)
    in
        -- Luego se revisa si alguna de las simplificaciones anteriores no es igual a la entrada original ya que esto significa que la simplificacion fue exitosa
        -- En este caso se repite el proceso hasta que el algoritmo no pueda simplificar mas
        if not(abs == Conjuncion prop1 prop2) then simpl abs
        else if not(idem == Conjuncion prop1 prop2) then simpl idem
        else if not(neu == Conjuncion prop1 prop2) then simpl neu
        else if not(abs1 == prop1) then simpl (Conjuncion abs1 prop2)
        else if not(idem1 == prop1) then simpl (Conjuncion idem1 prop2)
        else if not(neu1 == prop1) then simpl (Conjuncion neu1 prop2)
        else if not(abs2 == prop2) then simpl (Conjuncion prop1 abs2)
        else if not(idem2 == prop2) then simpl (Conjuncion prop1 idem2)
        else if not(neu2 == prop2) then simpl (Conjuncion prop1 neu2)
        else Conjuncion prop1 prop2     -- Si ninguna simplificacion fue exitos, entonces se retorna la entrada tal cual
-- El procedimiento es el mismo cuando la entrada es una disyuncion
simpl (Disyuncion prop1 prop2) =
    let
        abs = (absorcion (Disyuncion prop1 prop2))
        idem = (idempotencia (Disyuncion prop1 prop2))
        neu = (neutro (Disyuncion prop1 prop2))
        abs1 = (absorcion prop1)
        idem1 = (idempotencia prop1)
        neu1 = (neutro prop1)
        abs2 = (absorcion prop2)
        idem2 = (idempotencia prop2)
        neu2 = (neutro prop2)
    in
        if not(abs == Disyuncion prop1 prop2) then simpl abs
        else if not(idem == Disyuncion prop1 prop2) then simpl idem
        else if not(neu == Disyuncion prop1 prop2) then simpl neu
        else if not(abs1 == prop1) then simpl (Disyuncion abs1 prop2)
        else if not(idem1 == prop1) then simpl (Disyuncion idem1 prop2)
        else if not(neu1 == prop1) then simpl (Disyuncion neu1 prop2)
        else if not(abs2 == prop2) then simpl (Disyuncion prop1 abs2)
        else if not(idem2 == prop2) then simpl (Disyuncion prop1 idem2)
        else if not(neu2 == prop2) then simpl (Disyuncion prop1 neu2)
        else Disyuncion prop1 prop2
-- Cuando la entrada es una implicacion o una equivalencia, se intenta simplificar sus componentes
simpl (Implicacion prop1 prop2) = Implicacion (simpl(prop1)) (simpl(prop2))
simpl (Equivalencia prop1 prop2) = Equivalencia (simpl(prop1)) ( simpl(prop2))