module FuncAux (esConstante, esNegacion, esConjuncion, esDisyuncion, getProp1, getProp2) where
import Sintax

-- Funcion que determina si una proposicion es una constante
esConstante :: Proposicion -> Bool
esConstante (Constante valor) = True
esConstante _ = False

-- Funcion que determina si una proposicion es una negacion
esNegacion :: Proposicion -> Bool
esNegacion (Negacion prop) = True
esNegacion _ = False

-- Funcion que determina si una proposicion es una conjuncion
esConjuncion :: Proposicion -> Bool
esConjuncion (Conjuncion prop1 prop2) = True
esConjuncion _ = False

-- Funcion que determina si una proposicion es una disyuncion
esDisyuncion :: Proposicion -> Bool
esDisyuncion (Disyuncion prop1 prop2) = True
esDisyuncion _ = False

-- Funcion que retorna la primera proposicion dentro de una proposicion compuesta
-- En caso de consstantes y variables, se retorna la proposicion tal cual
-- En caso de negacion, se retorna la proposicion sin negar
getProp1 :: Proposicion -> Proposicion
getProp1 (Constante valor) = Constante valor
getProp1 (Variable nombre) = Variable nombre
getProp1 (Negacion prop) = prop
getProp1 (Conjuncion prop1 prop2) = prop1
getProp1 (Disyuncion prop1 prop2) = prop1
getProp1 (Implicacion prop1 prop2) = prop1
getProp1 (Equivalencia prop1 prop2) = prop1

-- Funcion que retorna la segunda proposicion dentro de una proposicion compuesta
-- En caso de consstantes y variables, se retorna la proposicion tal cual
-- En caso de negacion, se retorna la proposicion sin negar
getProp2 :: Proposicion -> Proposicion
getProp2 (Constante valor) = Constante valor
getProp2 (Variable nombre) = Variable nombre
getProp2 (Negacion prop) = prop
getProp2 (Conjuncion prop1 prop2) = prop2
getProp2 (Disyuncion prop1 prop2) = prop2
getProp2 (Implicacion prop1 prop2) = prop2
getProp2 (Equivalencia prop1 prop2) = prop2