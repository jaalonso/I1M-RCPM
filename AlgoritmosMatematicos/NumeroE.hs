-- NumeroE.hs
-- Cálculo del valor aproximado del número e.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module NumeroE where

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función aproxE tal que (aproXE n) es la
-- lista cuyos elementos son los términos de la sucesión (1+1/m)**m
-- desde 1 hasta n. Por ejemplo, 
--    aproxE 1 == [2.0]
--    aproxE 4 == [2.0,2.25,2.37037037037037,2.44140625]
-- ---------------------------------------------------------------------

aproxE n = [(1+1/m)**m | m <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. ¿Cuál es el límite de la sucesión (1+1/m)**m ?
-- ---------------------------------------------------------------------

-- El límite de la sucesión es el número e. 

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función errorE tal que (errorE x) es el
-- menor número de términos de la sucesión (1+1/m)**m necesarios para
-- obtener su límite con un error menor que x. Por ejemplo,
--    errorAproxE 0.1    ==  13.0
--    errorAproxE 0.01   ==  135.0
--    errorAproxE 0.001  ==  1359.0
-- Indicación: En Haskell, e se calcula como (exp 1).
-- ---------------------------------------------------------------------

errorAproxE x = head [m | m <- [1..], abs((exp 1) - (1+1/m)**m) < x]

