module Main where

    --imports de modulos
import Sintax
import Simpl
import Taut
import Fnd
import Fnc

main :: IO()
main = do
    
    --Ejemplos de formas normales disyuntivas
    let test1 = fnd (vp /\ (vq .=> vr))
    let res = imprimir test1

    do {
        putStr "\nfnc (vp /\\ (vq .=> vr))   : \n" ; print res ;
        putStr "\n\n";
    }