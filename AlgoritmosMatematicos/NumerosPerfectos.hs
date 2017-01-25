-- NumerosPerfectos.hs
-- Números perfectos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module NumerosPerfectos where

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un entero positivo es perfecto si es igual a la suma de
-- sus factores, excluyendo el propio número. 
-- 
-- Definir por comprensión la función 
--    perfectos :: Int -> [Int]
-- tal que (perfectos n) es la lista de todos los números perfectos
-- menores que n. Por ejemplo, 
--    perfectos 500  ==  [6,28,496]
-- Indicación: Usar la función factores del tema 5.
-- ---------------------------------------------------------------------

-- La función factores del tema es
factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

-- La definición es
perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], sum (init (factores x)) == x]
