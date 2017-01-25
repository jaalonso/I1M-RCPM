-- C�lculo del n�mero pi mediante el m�todo de Montecarlo.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

------------------------------------------------------------------------
-- � Introducci�n                                                     --
------------------------------------------------------------------------

-- El objetivo de esta relaci�n de ejercicios es el uso de los n�meros
-- aleatorios para calcular el n�mero pi mediante el m�todo de
-- Montecarlo. Un ejemplo del m�todo se puede leer en el art�culo de
-- Pablo Rodr�guez "Calculando Pi con gotas de lluvia" que se encuentra
-- en http://bit.ly/1cNfSR0 

-- ---------------------------------------------------------------------
-- � Librer�as auxiliares                                             --
-- ---------------------------------------------------------------------

import System.Random
import System.IO.Unsafe

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    aleatorio :: Random t => t -> t -> t
-- tal que (aleatorio a b) es un n�mero aleatorio entre a y b. Por
-- ejemplo, 
--    ghci> aleatorio 0 1000
--    681
--    ghci> aleatorio 0 1000
--    66
-- ---------------------------------------------------------------------

aleatorio :: Random t => t -> t -> t
aleatorio a b = unsafePerformIO $ 
                getStdRandom (randomR (a,b))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    aleatorios :: Random t => t -> t -> [t]
-- (aleatorios m n) es una lista infinita de n�meros aleatorios entre m y
-- n. Por ejemplo, 
--    ghci> take 20 (aleatorios 2 9)
--    [6,5,3,9,6,3,6,6,2,7,9,6,8,6,2,4,2,6,9,4]
--    ghci> take 20 (aleatorios 2 9)
--    [3,7,7,5,7,7,5,8,6,4,7,2,8,8,2,8,7,6,5,5]
-- ---------------------------------------------------------------------

aleatorios :: Random t => t -> t -> [t]
aleatorios m n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    puntosDelCuadrado :: [(Double,Double)]
-- tal que puntosDelCuadrado es una lista infinita de puntos del
-- cuadrado de v�rtices opuestos (-1,-1) y (1,1). Por ejemplo,
--    ghci> take 3 puntosDelCuadrado
--    [(0.5389481918223398,0.9385662370820778),
--     (-0.419123718392838,0.9982440984579455),
--     (0.5610432040657063,-0.7648360614536891)]
-- ---------------------------------------------------------------------

puntosDelCuadrado :: [(Double,Double)]
puntosDelCuadrado = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--    puntosEnElCirculo :: [(Double,Double)] -> Int
-- tal que (puntosEnElCirculo xs) es el n�mero de puntos de la lista xs
-- que est�n en el c�rculo de centro (0,0) y radio 1.
--    ghci> puntosEnElCirculo [(1,0), (0.5,0.9), (0.2,-0.3)]
--    2
-- ---------------------------------------------------------------------

puntosEnElCirculo :: [(Double,Double)] -> Int
puntosEnElCirculo xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--    calculoDePi :: Int -> Double
-- tal que (calculoDePi n) es el c�lculo del n�mero pi usando n puntos
-- aleatorios (la probabilidad de que est�n en el c�rculo es pi/4). Por
-- ejemplo, 
--    ghci> calculoDePi 1000
--    3.076
--    ghci> calculoDePi 10000
--    3.11
--    ghci> calculoDePi 100000
--    3.13484
-- ---------------------------------------------------------------------

calculoDePi :: Int -> Double
calculoDePi n = undefined

