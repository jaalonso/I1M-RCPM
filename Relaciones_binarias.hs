-- I1M 2015-16: Relaci�n 27 (11 de marzo de 2016)
-- Relaciones binarias homog�neas con la librer�a Data.Set.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n de ejercicios es definir propiedades y
-- operaciones sobre las relaciones binarias (homog�neas) usando la
-- librer�a Data.Set.
--
-- Como referencia se puede usar el art�culo de la wikipedia
-- http://bit.ly/HVHOPS 

-- ---------------------------------------------------------------------
-- � Pragmas                                                          --
-- ---------------------------------------------------------------------

{-# LANGUAGE OverlappingInstances, 
             TypeSynonymInstances, 
             FlexibleInstances #-}

-- ---------------------------------------------------------------------
-- � Librer�as auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Set

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una relaci�n binaria R sobre un conjunto A puede
-- representar mediante un par (xs,ps) donde xs es el conjunto de los
-- elementos de A (el universo de R) y ps es el conjunto de pares de R
-- (el grafo de R). Definir el tipo de dato (Rel a) para representar las
-- relaciones binarias sobre a.  
-- ---------------------------------------------------------------------

type Rel a = (Set a, Set (a,a))

-- ---------------------------------------------------------------------
-- Nota. En los ejemplos usaremos las siguientes relaciones binarias: 
--    r1, r2, r3 :: Rel Int
--    r1 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (2,7)])
--    r2 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,7)])
--    r3 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,6)])
-- ---------------------------------------------------------------------

r1, r2, r3 :: Rel Int
r1 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (2,7)])
r2 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,7)])
r3 = (fromList [1..9],fromList [(1,3), (2,6), (8,9), (3,6)])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    universo :: Ord a => Rel a -> Set a
-- tal que (universo r) es el universo de la relaci�n r. Por ejemplo, 
--    universo r1  ==  fromList [1,2,3,4,5,6,7,8,9]
-- ---------------------------------------------------------------------

universo :: Ord a => Rel a -> Set a
universo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    grafo :: Ord a => Rel a -> [(a,a)]
-- tal que (grafo r) es el grafo de la relaci�n r. Por ejemplo, 
--    grafo r1  ==  fromList [(1,3),(2,6),(2,7),(8,9)]
-- ---------------------------------------------------------------------

grafo :: Ord a => Rel a -> Set (a,a)
grafo = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--    reflexiva :: Ord a => Rel a -> Bool
-- tal que (reflexiva r) se verifica si la relaci�n r es reflexiva. Por
-- ejemplo, 
--    ghci> reflexiva (fromList [1,3], fromList [(1,1),(1,3),(3,3)])
--    True
--    ghci> reflexiva (fromList [1,2,3], fromList [(1,1),(1,3),(3,3)])
--    False
-- ---------------------------------------------------------------------

reflexiva :: Ord a => Rel a -> Bool
reflexiva = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la funci�n
--    simetrica :: Ord a => Rel a -> Bool
-- tal que (simetrica r) se verifica si la relaci�n r es sim�trica. Por
-- ejemplo, 
--    ghci> simetrica (fromList [1,3], fromList [(1,1),(1,3),(3,1)])
--    True
--    ghci> simetrica (fromList [1,3], fromList [(1,1),(1,3),(3,2)])
--    False
--    ghci> simetrica (fromList [1,3], fromList [])
--    True
-- ---------------------------------------------------------------------

simetrica :: Ord a => Rel a -> Bool
simetrica = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funci�n
--    subconjunto :: Ord a => Set a -> Set a -> Bool
-- tal que (subconjunto c1 c2) se verifica si c1 es un subconjunto de
-- c2. Por ejemplo,
--    subconjunto (fromList [1,3]) (fromList [3,1,5])  ==  True
--    subconjunto (fromList [3,1,5]) (fromList [1,3])  ==  False
-- ---------------------------------------------------------------------

subconjunto :: Ord a => Set a -> Set a -> Bool
subconjunto = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
--    composicion :: Ord a => Rel a -> Rel a -> Rel a
-- tal que (composicion r s) es la composici�n de las relaciones r y
-- s. Por ejemplo, 
--    ghci> let r1 = (fromList [1,2], fromList [(1,2),(2,2)])
--    ghci> let r2 = (fromList [1,2], fromList [(2,1)])
--    ghci> let r3 = (fromList [1,2], fromList [(1,1)])
--    ghci> composicion r1 r2
--    (fromList [1,2],fromList [(1,1),(2,1)])
--    ghci> composicion r1 r3
--    (fromList [1,2,3,4,5,6,7,8,9],fromList [])
-- ---------------------------------------------------------------------

composicion :: Ord a => Rel a -> Rel a -> Rel a
composicion = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n
--    transitiva :: Ord a => Rel a -> Bool
-- tal que (transitiva r) se verifica si la relaci�n r es transitiva. 
-- Por ejemplo,
--    ghci> transitiva (fromList [1,3,5],fromList [(1,1),(1,3),(3,1),(3,3),(5,5)])
--    True
--    ghci> transitiva (fromList [1,3,5],fromList [(1,1),(1,3),(3,1),(5,5)])
--    False
-- ---------------------------------------------------------------------

transitiva :: Ord a => Rel a -> Bool
transitiva = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n
--    esEquivalencia :: Ord a => Rel a -> Bool
-- tal que (esEquivalencia r) se verifica si la relaci�n r es de
-- equivalencia. Por ejemplo,
--    ghci> esEquivalencia (fromList [1,3,5],
--                          fromList [(1,1),(1,3),(3,1),(3,3),(5,5)])
--    True
--    ghci> esEquivalencia (fromList [1,2,3,5],
--                          fromList [(1,1),(1,3),(3,1),(3,3),(5,5)])
--    False
--    ghci> esEquivalencia (fromList [1,3,5],
--                          fromList [(1,1),(1,3),(3,3),(5,5)])
--    False
-- ---------------------------------------------------------------------

esEquivalencia :: Ord a => Rel a -> Bool
esEquivalencia r = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--    irreflexiva :: Ord a => Rel a -> Bool
-- tal que (irreflexiva r) se verifica si la relaci�n r es irreflexiva;
-- es decir, si ning�n elemento de su universo est� relacionado con 
-- �l mismo. Por ejemplo,
--    ghci> irreflexiva (fromList [1,2,3],fromList [(1,2),(2,1),(2,3)])
--    True
--    ghci> irreflexiva (fromList [1,2,3],fromList [(1,2),(2,1),(3,3)])
--    False
-- ---------------------------------------------------------------------

irreflexiva :: Ord a => Rel a -> Bool
irreflexiva = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la funci�n
--    antisimetrica :: Ord a => Rel a -> Bool
-- tal que (antisimetrica r) se verifica si la relaci�n r es
-- antisim�trica; es decir, si (x,y) e (y,x) est�n relacionado, entonces
-- x=y. Por ejemplo,
--    antisimetrica (fromList [1,2],fromList [(1,2)])        ==  True
--    antisimetrica (fromList [1,2],fromList [(1,2),(2,1)])  ==  False
--    antisimetrica (fromList [1,2],fromList [(1,1),(2,1)])  ==  True
-- ---------------------------------------------------------------------

antisimetrica :: Ord a => Rel a -> Bool
antisimetrica = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la funci�n
--    total :: Ord a => Rel a -> Bool
-- tal que (total r) se verifica si la relaci�n r es total; es decir, si
-- para cualquier par x, y de elementos del universo de r, se tiene que
-- x est� relacionado con y � y et� relacionado con x. Por ejemplo,
--    total (fromList [1,3],fromList [(1,1),(3,1),(3,3)])  ==  True
--    total (fromList [1,3],fromList [(1,1),(3,1)])        ==  False
--    total (fromList [1,3],fromList [(1,1),(3,3)])        ==  False
-- ---------------------------------------------------------------------

total :: Ord a => Rel a -> Bool
total = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13. Comprobar con QuickCheck que las relaciones totales son
-- reflexivas. 
-- ---------------------------------------------------------------------

prop_total_reflexiva :: Rel Int -> Property
prop_total_reflexiva r = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- � Clausuras                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la funci�n
--    clausuraReflexiva :: Ord a => Rel a -> Rel a  
-- tal que (clausuraReflexiva r) es la clausura reflexiva de r; es
-- decir, la menor relaci�n reflexiva que contiene a r. Por ejemplo,
--    ghci> clausuraReflexiva (fromList [1,3], fromList [(1,1),(3,1)])
--    (fromList [1,3],fromList [(1,1),(3,1),(3,3)])
-- ---------------------------------------------------------------------

clausuraReflexiva :: Ord a => Rel a -> Rel a  
clausuraReflexiva = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que clausuraReflexiva es
-- reflexiva. 
-- ---------------------------------------------------------------------

prop_ClausuraReflexiva :: Rel Int -> Bool
prop_ClausuraReflexiva r = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la funci�n
--    clausuraSimetrica :: Ord a => Rel a -> Rel a  
-- tal que (clausuraSimetrica r) es la clausura sim�trica de r; es
-- decir, la menor relaci�n sim�trica que contiene a r. Por ejemplo,
--    ghci> clausuraSimetrica (fromList [1,3,5],fromList [(1,1),(3,1),(1,5)])
--    (fromList [1,3,5],fromList [(1,1),(1,3),(1,5),(3,1),(5,1)])
-- ---------------------------------------------------------------------

clausuraSimetrica :: Ord a => Rel a -> Rel a  
clausuraSimetrica = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17. Comprobar con QuickCheck que clausuraSimetrica es
-- sim�trica. 
-- ---------------------------------------------------------------------

prop_ClausuraSimetrica :: Rel Int -> Bool
prop_ClausuraSimetrica r = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la funci�n
--    clausuraTransitiva :: Ord a => Rel a -> Rel a  
-- tal que (clausuraTransitiva r) es la clausura transitiva de r; es
-- decir, la menor relaci�n transitiva que contiene a r. Por ejemplo,
--    ghci> clausuraTransitiva (fromList [1..6],fromList [(1,2),(2,5),(5,6)])
--    (fromList [1,2,3,4,5,6],fromList [(1,2),(1,5),(1,6),(2,5),(2,6),(5,6)])
-- ---------------------------------------------------------------------

clausuraTransitiva :: Ord a => Rel a -> Rel a  
clausuraTransitiva = undefined
          
-- ---------------------------------------------------------------------
-- Ejercicio 19. Comprobar con QuickCheck que clausuraTransitiva es
-- transitiva. 
-- ---------------------------------------------------------------------

prop_ClausuraTransitiva :: Rel Int -> Bool
prop_ClausuraTransitiva r = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- � Generador de relaciones                                          --
-- ---------------------------------------------------------------------

-- genSet es un generador de relaciones binarias. Por ejemplo,
--    ghci> sample genRel
--    (fromList [0],fromList [])
--    (fromList [-1,1],fromList [(-1,1)])
--    (fromList [-3,-2],fromList [])
--    (fromList [-2,0,1,6],fromList [(0,0),(6,0)])
--    (fromList [-7,0,2],fromList [(-7,0),(2,0)])
--    (fromList [2,11],fromList [(2,2),(2,11),(11,2),(11,11)])
--    (fromList [-4,-2,1,4,5],fromList [(1,-2),(1,1),(1,5)])
--    (fromList [-4,-3,-2,6,7],fromList [(-3,-4),(7,-3),(7,-2)])
--    (fromList [-9,-7,0,10],fromList [(10,-9)])
--    (fromList [-10,3,8,10],fromList [(3,3),(10,-10)])
--    (fromList [-10,-9,-7,-6,-5,-4,-2,8,12],fromList [])
genRel :: (Arbitrary a, Integral a) => Gen (Rel a)
genRel = do xs <- listOf1 arbitrary
            ys <- listOf (elements [(x,y) | x <- xs, y <- xs])
            return (fromList xs, fromList ys)

instance (Arbitrary a, Integral a) => Arbitrary (Rel a) where
    arbitrary = genRel
