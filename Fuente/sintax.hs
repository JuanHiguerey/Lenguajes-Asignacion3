module Sintax (Proposicion (..), imprimir, (¬), (/\), (\/), (.=>), (<=>), vp, vq, vr) where


--Definicion del tipo de dato Proposicion
data Proposicion =
    Constante       Bool
    | Variable      String
    | Negacion      Proposicion
    | Conjuncion    Proposicion Proposicion
    | Disyuncion    Proposicion Proposicion
    | Implicacion   Proposicion Proposicion
    | Equivalencia  Proposicion Proposicion
    deriving Show
;

instance Eq Proposicion where
  (==) (Constante valor1) (Constante valor2) = if (valor1 == valor2) then True else False
  (==) (Variable nombre1) (Variable nombre2) = if (nombre1 == nombre2) then True else False
  (==) (Negacion prop1) (Negacion prop2) = if (prop1 == prop2) then True else False
  (==) (Conjuncion prop1a prop1b) (Conjuncion prop2a prop2b) = if ((prop1a == prop2a) && (prop2b == prop2b)) then True else False
  (==) (Disyuncion prop1a prop1b) (Disyuncion prop2a prop2b) = if ((prop1a == prop2a) && (prop2b == prop2b)) then True else False
  (==) (Implicacion prop1a prop1b) (Implicacion prop2a prop2b) = if ((prop1a == prop2a) && (prop2b == prop2b)) then True else False
  (==) (Equivalencia prop1a prop1b) (Equivalencia prop2a prop2b) = if ((prop1a == prop2a) && (prop2b == prop2b)) then True else False
  (==) _ _ = False


--Devuelve un string de la estructura de la proposicion dada
imprimir prop = 
    case prop of
        Constante False          -> "false"
        Constante True           -> "true"
        Variable nombre          -> nombre
        Negacion prop1           -> "negacion (" ++ imprimir  prop1 ++ ")"
        Conjuncion prop1 prop2   -> "conjuncion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
        Disyuncion prop1 prop2   -> "disyuncion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
        Implicacion prop1 prop2  -> "implicacion (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
        Equivalencia prop1 prop2 -> "equivalencia (" ++ imprimir prop1 ++ ", " ++ imprimir prop2 ++ ")"
;


-------------------------Operadores logicos--------------------------------------

--el simbolo de la negacion tiene que ir entre parentesis y la proposicion tambien
--ejemplo: (¬)(Variable "a")
(¬) = Negacion;

infixl 7 /\;
(/\) = Conjuncion;

infixl 6 \/;
(\/) = Disyuncion;

infixr 5 .=>;
(.=>) = Implicacion;

infixl 4 <=>;
(<=>) = Equivalencia;


-- Variables tradicionales P, Q, y R
vp = Variable "P"
vq = Variable "Q"
vr = Variable "R"
