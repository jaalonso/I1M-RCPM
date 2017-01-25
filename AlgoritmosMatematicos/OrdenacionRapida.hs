-- OrdenacionRapida.hs
-- Algoritmo de ordenación rápida.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module OrdenacionRapida where

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    ordena :: Ord a => [a] -> [a]
-- tal que (ordena xs) es la lista obtenida ordenando los elementos de
-- xs mediante el algoritmo de ordenación rápida. Por ejemplo, 
--    ordena [4,6,2,5,3] == [2,3,4,5,6]
--    ordena "deacb"     == "abcde"
-- ---------------------------------------------------------------------

ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena (x:xs) = 
    (ordena menores) ++ [x] ++ (ordena mayores)
    where menores = [a | a <- xs, a <= x]
          mayores = [b | b <- xs, b > x]

-- ---------------------------------------------------------------------
-- Referencia. El algoritmo se ha estudiado en el tema 1.
-- ---------------------------------------------------------------------