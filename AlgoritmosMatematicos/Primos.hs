-- Primos.hs
-- Lista de primeros primos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module Primos where

import Primo

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    primos :: Int -> [Int]
-- tal que (primos n) es la lista de los primos menores o iguales que
-- n. Por ejemplo, 
--    primos 31  == [2,3,5,7,11,13,17,19,23,29,31]  
-- ---------------------------------------------------------------------

primos :: Int -> [Int]
primos n = [x | x <- [2..n], primo x]

