-- Bezout.hs
-- La identidad de Bezout.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    bezout :: Integer -> Integer -> (Integer, Integer)
-- tal que (bezout a b) es un par de números x e y tal que a*x+b*y es el
-- máximo común divisor de a y b. Por ejemplo,
--    bezout 12 30  ==  (-2,1)
-- Indicación: Se puede usar la función quotRem tal que (quotRem x y) es
-- el par formado por el cociente y el resto de dividir x entre y.
-- ---------------------------------------------------------------------

-- Ejemplo de cálculo
--    a  b   q r  
--    12 30  0 12  (-1,1-0*(-1)) = (-1,1)
--    30 12  1 18  (1,0-1*1)     = (1,-1)
--    12 18  0 18  (0,1-0*0)     = (0,1)
--    18 18  1  0  (1,0) 

bezout :: Integer -> Integer -> (Integer, Integer)
bezout _ 0 = (1,0)
bezout _ 1 = (0,1)
bezout a b = (y, x - q*y)
    where (x,y) = bezout b r
          (q,r) = quotRem a b

-- ---------------------------------------------------------------------
-- Ejercicio 2. Comprobar con QuickCheck que si a>0, b>0 y 
-- (x,y) es el valor de (bezout a b), entonces a*x+b*y es igual al
-- máximo común divisor de a y b.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_Bezout :: Integer -> Integer -> Property
prop_Bezout a b = a>0 && b>0 ==> a*x+b*y == gcd a b
    where (x,y) = bezout a b          

-- La comprobación es
--   Main> quickCheck prop_Bezout
--   OK, passed 100 tests.
