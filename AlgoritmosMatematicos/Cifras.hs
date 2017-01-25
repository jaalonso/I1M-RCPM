-- Cifras.hs
-- Cifras de un número.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensión, la función
--    cifras :: Integer -> [Int]
-- tal que (cifras n) es la lista de los cifras del número n. Por
-- ejemplo, 
--    cifras 320274  ==  [3,2,0,2,7,4]
-- Indicación: Usar las funciones show y read.
-- ---------------------------------------------------------------------

cifras :: Integer -> [Integer]
cifras n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por recursión, la función
--    cifrasR :: Integer -> [Int]
-- tal que (cifrasR n) es la lista de los cifras del número n. Por
-- ejemplo, 
--    cifrasR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

cifrasR :: Integer -> [Integer]
cifrasR n = reverse (cifrasR' n)

cifrasR' n
    | n < 10    = [n]
    | otherwise = (n `rem` 10) : cifrasR' (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por comprensión, la función 
--    listaNumeroC :: [Integer] -> Integer
-- tal que (listaNumeroC xs) es el número formado por las cifras xs. Por
-- ejemplo, 
--    listaNumeroC [5]        == 5
--    listaNumeroC [1,3,4,7]  == 1347
--    listaNumeroC [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroC :: [Integer] -> Integer
listaNumeroC xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4, Definir, por recursión, la función 
--    listaNumeroR :: [Integer] -> Integer
-- tal que (listaNumeroR xs) es el número formado por las cifras xs. Por
-- ejemplo, 
--    listaNumeroR [5]        == 5
--    listaNumeroR [1,3,4,7]  == 1347
--    listaNumeroR [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroR :: [Integer] -> Integer
listaNumeroR xs = listaNumeroR' (reverse xs)

listaNumeroR' :: [Integer] -> Integer
listaNumeroR' [x]    = x
listaNumeroR' (x:xs) = x + 10 * (listaNumeroR' xs)
