-- Factorizacion.hs
-- Factorización de un número.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module Factorizacion where

import Factores
import Primo

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursión, la función 
--    mayorExponente :: Int -> Int -> Int 
-- tal que (mayorExponente a b) es el exponente de la mayor potencia de
-- a que divide b. Por ejemplo,
--    mayorExponente 2 8    ==  3
--    mayorExponente 2 9    ==  3
--    mayorExponente 5 100  ==  2
--    mayorExponente 2 60   ==  2
-- ---------------------------------------------------------------------

mayorExponente :: Int -> Int -> Int 
mayorExponente a b
    | mod b a /= 0 = 0
    | otherwise    = 1 + mayorExponente a (b `div` a)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    factoresPrimos :: Int -> [Int]
-- tal que (factoresPrimos n) es la lista de los factores primos de
-- n. Por ejemplo,  
--    factoresPrimos 60  ==  [2,3,5]
-- ---------------------------------------------------------------------

factoresPrimos :: Int -> [Int]
factoresPrimos n = [x | x <- factores n, primo x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    factorizacion :: Int -> [(Int,Int)]
-- tal que (factorizacion n) es la factorización de n. Por ejemplo,  
--    factorizacion 60  ==  [(2,2),(3,1),(5,1)]
-- ---------------------------------------------------------------------

factorizacion :: Int -> [(Int,Int)]
factorizacion n = [(x,mayorExponente x n) | x <- factoresPrimos n]

-- Una forma alternativa es la que se define a continuación

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    menorFactor :: Int -> Int
-- tal que (menorFactor n) es el menor factor primo de n. Por ejemplo,
--    menorFactor 15  ==  3
-- ---------------------------------------------------------------------

menorFactor :: Int -> Int
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    factorizacion' :: Int -> [Int]
-- tal que (factorizacion' n) es la lista de todos los factores primos
-- de n; es decir, es una lista de números primos cuyo producto es
-- n. Por ejemplo,
--    factorizacion' 300  ==  [2,2,3,5,5]
-- ---------------------------------------------------------------------

factorizacion' :: Int -> [Int]
factorizacion' n | primo n   = [n]
                 | otherwise = x : factorizacion' (div n x)
                 where x = menorFactor n

