-- Factores.hs
-- Cálculo de los factores de un número.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module Factores where

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    factores :: Int -> [Int]
-- tal que (factores n) es la lista de los factores del número n. Por
-- ejemplo, 
--    factores 30  ==  [1,2,3,5,6,10,15,30]  
-- ---------------------------------------------------------------------

factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]
