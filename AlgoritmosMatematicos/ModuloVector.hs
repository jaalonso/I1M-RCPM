-- ModuloVector.hs
-- Cálculo del módulo de un vector.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module ModuloVector where

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función modulo tal que (modulo v) es el
-- módulo del vector v. Por ejemplo,
--    modulo (3,4)  ==  5.0
-- ---------------------------------------------------------------------

modulo (x,y) = sqrt(x^2+y^2)
