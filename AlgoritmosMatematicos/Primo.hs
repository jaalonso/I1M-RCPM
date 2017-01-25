-- Primo.hs
-- Verificación si un número es primo.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module Primo where

import Factores 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    primo :: Int -> Bool
-- tal que (primo n) se verifica si n es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True  
-- ---------------------------------------------------------------------

primo :: Int -> Bool
primo n = factores n == [1, n]
