-- RaicesEcuacionCuadratica.hs
-- Raíces de la ecuación de segundo grado.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module RaicesEcuacionCuadratica where 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función raices de forma que raices a b c
-- devuelve la lista de las raices reales de la ecuación 
-- ax^2 + bx + c = 0. Por ejemplo, 
--    raices 1 (-2) 1 == [1.0,1.0]
--    raices 1 2 3    == [-1.0,-2.0]
-- Escribir distintas definiciones de la función raices.
-- ---------------------------------------------------------------------

raices_1 :: Double -> Double -> Double -> [Double]
raices_1 a b c = [(-b+d)/t,(-b-d)/t]
    where d = sqrt (b^2 - 4*a*c)
          t = 2*a

raices_2 :: Double -> Double -> Double -> [Double]
raices_2 a b c
    | d >= 0    = [(-b+e)/(2*a), (-b-e)/(2*a)]
    | otherwise = error "No tine raices reales"
    where d = b^2-4*a*c
          e = sqrt d 

-- Sesión:
-- *Main> raices_2 1 (-2) 1
-- [1.0,1.0]
-- *Main> raices_2 1 2 3
-- *** Exception: No tine raices reales
-- *Main> raices_2 1 3 2
-- [-1.0,-2.0]
-- *Main> raices_1 1 (-2) 1
-- [1.0,1.0]
-- *Main> raices_1 1 2 3
-- [NaN,NaN]
-- *Main> raices_1 1 3 2
-- [-1.0,-2.0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Comprobar con QuickCheck si todas las definiciones
-- coinciden. 
-- ---------------------------------------------------------------------

prop_raices12 :: Double -> Double -> Double -> Bool
prop_raices12 a b c =
  raices_2 a b c == raices_1 a b c

-- *Main> quickCheck prop_raices12
-- Falsifiable, after 0 tests:
-- 0.0
-- 0.0
-- 0.0

prop_raices12_b :: Double -> Double -> Double -> Property
prop_raices12_b a b c =
  a/=0 && not(b==0 &&c==0) && (b^2 - 4*a*c) >= 0
  ==>
  raices_2 a b c == raices_1 a b c

-- *Main> quickCheck prop_raices12_b
-- OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que las definiciones son
-- correctas; es decir, que los elementos del resultado son raices de la
-- ecuación.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_raices1_correcta a b c =
  a /= 0 && (b^2-4*a*c) >= 0 
  ==> (a*x^2 + b*x + c) ~= 0 && (a*y^2 + b*y + c) ~= 0 
  where [x,y]  = raices_1 a b c
        x ~= y = abs(x-y)<0.001

-- La comprobación es
--    *Main> quickCheck prop_raices1_correcta
--    +++ OK, passed 100 tests.

