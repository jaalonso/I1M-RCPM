-- LimiteSenx.hs
-- Cálculo del límite de sen(x)/x cuando x tiende a cero.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función aproxLimSeno tal que 
-- (aproxLimSeno n) es la lista cuyos elementos son los términos de la
-- sucesión  
--    sen(1/m) 
--    --------
--      1/m 
-- desde 1 hasta n. Por ejemplo,
--    aproxLimSeno 1 == [0.8414709848078965]
--    aproxLimSeno 2 == [0.8414709848078965,0.958851077208406]
-- ---------------------------------------------------------------------

aproxLimSeno n = [sin(1/m)/(1/m) | m <- [1..n]] 

-- ---------------------------------------------------------------------
-- Ejercicio 2. ¿Cuál es el límite de la sucesión sen(1/m)/(1/m) ?
-- ---------------------------------------------------------------------

-- El límite es 1.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función errorLimSeno tal que 
-- (errorLimSeno x) es el menor número de términos de la sucesión 
-- sen(1/m)/(1/m) necesarios para obtener su límite con un error menor
-- que x. Por ejemplo, 
--    errorLimSeno 0.1     ==   2.0
--    errorLimSeno 0.01    ==   5.0
--    errorLimSeno 0.001   ==  13.0
--    errorLimSeno 0.0001  ==  41.0
-- ---------------------------------------------------------------------

errorLimSeno x = head [m | m <- [1..], abs(1 - sin(1/m)/(1/m)) < x]

