-- ProductoCartesiano.hs
-- Producto cartesiano.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module ProductoCartesiano where

-- ---------------------------------------------------------------------
-- Ejercicio 1. La función 
--    pares :: [a] -> [b] -> [(a,b)]
-- definida por
--    pares xs ys = [(x,y) | x <- xs, y <- ys]
-- toma como argumento dos listas y devuelve la listas de los pares con
-- el primer elemento de la primera lista y el segundo de la
-- segunda. Por ejemplo,
--    *Main> pares [1..3] [4..6]
--    [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- 
-- Definir, usando dos listas por comprensión con un generador cada una,
-- la función 
--    pares' :: [a] -> [b] -> [(a,b)]
-- tal que pares' sea equivalente a pares.
-- 
-- Comprobar con QuickCheck que las dos definiciones son equivalentes.
-- 
-- Indicación: Utilizar la función predefinida concat y encajar una
-- lista por comprensión dentro de la otra. 
-- ---------------------------------------------------------------------

-- La definición de pares es
pares :: [a] -> [b] -> [(a,b)]
pares xs ys = [(x,y) | x <- xs, y <- ys]

-- La redefinición de pares es
pares' :: [a] -> [b] -> [(a,b)]
pares' xs ys = concat [[(x,y) | y <- ys] | x <- xs]

-- La propiedad es
prop_pares :: (Eq a, Eq b) => [a] -> [b] -> Bool
prop_pares xs ys = pares xs ys == pares' xs ys

-- La comprobación es
--    *Main> quickCheck prop_pares
--    +++ OK, passed 100 