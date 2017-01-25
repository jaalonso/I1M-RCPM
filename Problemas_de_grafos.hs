-- I1M 2015-16: Relaci�n 36 (30 de abril de 2016) 
-- Problemas b�sicos con el TAD de los grafos.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relaci�n de ejercicios es definir funciones sobre 
-- el TAD de los grafos estudiado en el tema 22 
--    http://www.cs.us.es/~jalonso/cursos/i1m-15/temas/tema-22.html
-- 
-- Para realizar los ejercicios hay que tener instalada la librer�a I1M
-- que contiene la implementaci�n de TAD de los polinomios. Los pasos
-- para instalarla son los siguientes:
-- + Descargar el paquete I1M desde http://bit.ly/1pbnDqm
-- + Descomprimirlo (y se crea el directorio I1M-master.zip).
-- + Cambiar al directorio I1M-master.
-- + Ejecutar cabal install I1M.cabal
-- 
-- Otra forma es descargar, en el directorio de ejercicios, la
-- implementaci�n del TAD de grafos 
-- + GrafoConVectorDeAdyacencia que est� en http://bit.ly/1SQnG4S
-- + GrafoConMatrizDeAdyacencia que est� en http://bit.ly/1SQnGlB
-- 
-- Los m�dulos anteriores se encuentras en la p�gina de c�digos 
-- http://bit.ly/1SQnAKO
 
-- ---------------------------------------------------------------------
-- Importaci�n de librer�as                                           --
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Array
import Data.List (nub)
import Test.QuickCheck

-- Hay que elegir una librer�a 
import I1M.Grafo
-- import GrafoConVectorDeAdyacencia 
-- import GrafoConMatrizDeAdyacencia 

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Para los ejemplos se usar�n los siguientes grafos.
g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11 :: Grafo Int Int
g1 = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                         (2,4,55),(2,5,32),
                         (3,4,61),(3,5,44),
                         (4,5,93)]
g2 = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                        (2,4,55),(2,5,32),
                        (4,3,61),(4,5,93)]
g3 = creaGrafo D (1,3) [(1,2,0),(2,2,0),(3,1,0),(3,2,0)]
g4 = creaGrafo D (1,4) [(1,2,3),(2,1,5)]
g5 = creaGrafo D (1,1) [(1,1,0)]
g6 = creaGrafo D (1,4) [(1,3,0),(3,1,0),(3,3,0),(4,2,0)]
g7 = creaGrafo ND (1,4) [(1,3,0)]
g8 = creaGrafo D (1,5) [(1,1,0),(1,2,0),(1,3,0),(2,4,0),(3,1,0),
                        (4,1,0),(4,2,0),(4,4,0),(4,5,0)]
g9 = creaGrafo D (1,5) [(4,1,1),(4,3,2),(5,1,0)]
g10 = creaGrafo ND (1,3) [(1,2,1),(1,3,1),(2,3,1),(3,3,1)]
g11 = creaGrafo D (1,3) [(1,2,1),(1,3,1),(2,3,1),(3,3,1)]

-- ---------------------------------------------------------------------
-- Ejercicio 1. El grafo completo de orden n, K(n), es un grafo no
-- dirigido cuyos conjunto de v�rtices es {1,..n} y tiene una arista
-- entre cada par de v�rtices distintos. Definir la funci�n,
--    completo :: Int -> Grafo Int Int
-- tal que (completo n) es el grafo completo de orden n. Por ejemplo,
--    ghci> completo 4
--    G ND (array (1,4) [(1,[(2,0),(3,0),(4,0)]),
--                       (2,[(1,0),(3,0),(4,0)]),
--                       (3,[(1,0),(2,0),(4,0)]),
--                       (4,[(1,0),(2,0),(3,0)])])
-- ---------------------------------------------------------------------

completo :: Int -> Grafo Int Int
completo n = undefined 

-- ---------------------------------------------------------------------
-- Ejercicio 2. El ciclo de orden n, C(n), es un grafo no dirigido
-- cuyo conjunto de v�rtices es {1,...,n} y las aristas son
--    (1,2), (2,3), ..., (n-1,n), (n,1)
-- Definir la funci�n
--    grafoCiclo :: Int -> Grafo Int Int
-- tal que (grafoCiclo n) es el grafo ciclo de orden n. Por ejemplo,
--    ghci> grafoCiclo 3
--    G ND (array (1,3) [(1,[(3,0),(2,0)]),(2,[(1,0),(3,0)]),(3,[(2,0),(1,0)])])
-- ---------------------------------------------------------------------

grafoCiclo :: Int -> Grafo Int Int
grafoCiclo n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    nVertices :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nVertices g) es el n�mero de v�rtices del grafo g. Por
-- ejemplo, 
--    nVertices (completo 4)  ==  4
--    nVertices (completo 5)  ==  5
-- ---------------------------------------------------------------------

nVertices :: (Ix v,Num p) => Grafo v p ->  Int
nVertices = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n
--    noDirigido :: (Ix v,Num p) => Grafo v p ->  Bool
-- tal que (noDirigido g) se verifica si el grafo g es no dirigido. Por
-- ejemplo, 
--    noDirigido g1            ==  True
--    noDirigido g2            ==  False
--    noDirigido (completo 4)  ==  True
-- ---------------------------------------------------------------------

noDirigido :: (Ix v,Num p) => Grafo v p ->  Bool
noDirigido = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. En un un grafo g, los incidentes de un v�rtice v es el
-- conjuntos de v�rtices x de g para los que hay un arco (o una arista)
-- de x a v; es decir, que v es adyacente a x. Definir la funci�n
--    incidentes :: (Ix v,Num p) => (Grafo v p) -> v -> [v]
-- tal que (incidentes g v) es la lista de los v�rtices incidentes en el
-- v�rtice v. Por ejemplo,
--    incidentes g2 5  ==  [1,2,4]
--    adyacentes g2 5  ==  []
--    incidentes g1 5  ==  [1,2,3,4]
--    adyacentes g1 5  ==  [1,2,3,4]
-- --------------------------------------------------------------------- 

incidentes :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes g v = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. En un un grafo g, los contiguos de un v�rtice v es el
-- conjuntos de v�rtices x de g tales que x es adyacente o incidente con
-- v. Definir la funci�n
--    contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
-- tal que (contiguos g v) es el conjunto de los v�rtices de g contiguos
-- con el v�rtice v. Por ejemplo,
--    contiguos g2 5  ==  [1,2,4]
--    contiguos g1 5  ==  [1,2,3,4]
-- ---------------------------------------------------------------------

contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
contiguos g v = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n
--    lazos :: (Ix v,Num p) => Grafo v p -> [(v,v)]
-- tal que (lazos g) es el conjunto de los lazos (es decir, aristas
-- cuyos extremos son iguales) del grafo g. Por ejemplo, 
--    ghci> lazos g3
--    [(2,2)]
--    ghci> lazos g2
--    []
-- ---------------------------------------------------------------------

lazos :: (Ix v,Num p) => Grafo v p -> [(v,v)]
lazos g = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n
--    nLazos :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nLazos g) es el n�mero de lazos del grafo g. Por
-- ejemplo, 
--    nLazos g3  ==  1
--    nLazos g2  ==  0
-- ---------------------------------------------------------------------

nLazos :: (Ix v,Num p) => Grafo v p ->  Int
nLazos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funci�n
--    nAristas :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nAristas g) es el n�mero de aristas del grafo g. Si g es no
-- dirigido, las aristas de v1 a v2 y de v2 a v1 s�lo se cuentan una
-- vez. Por ejemplo, 
--    nAristas g1            ==  8
--    nAristas g2            ==  7
--    nAristas g10           ==  4
--    nAristas (completo 4)  ==  6
--    nAristas (completo 5)  ==  10
-- ---------------------------------------------------------------------

nAristas :: (Ix v,Num p) => Grafo v p ->  Int
nAristas g = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--    prop_nAristasCompleto :: Int -> Bool
-- tal que (prop_nAristasCompleto n) se verifica si el n�mero de aristas
-- del grafo completo de orden n es n*(n-1)/2 y, usando la funci�n,
-- comprobar que la propiedad se cumple para n de 1 a 20.
-- ---------------------------------------------------------------------

prop_nAristasCompleto :: Int -> Bool
prop_nAristasCompleto n = undefined

-- La comprobaci�n es
--    ghci> and [prop_nAristasCompleto n | n <- [1..20]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 11. El grado positivo de un v�rtice v de un grafo dirigido
-- g, es el n�mero de v�rtices de g adyacentes con v. Definir la funci�n  
--    gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (gradoPos g v) es el grado positivo del v�rtice v en el grafo
-- g. Por ejemplo,
--    gradoPos g1 5  ==  4
--    gradoPos g2 5  ==  0
--    gradoPos g2 1  ==  3
-- ---------------------------------------------------------------------

gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoPos g v = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. El grado negativo de un v�rtice v de un grafo dirigido
-- g, es el n�mero de v�rtices de g incidentes con v. Definir la funci�n  
--    gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (gradoNeg g v) es el grado negativo del v�rtice v en el grafo
-- g. Por ejemplo,
--    gradoNeg g1 5  ==  4
--    gradoNeg g2 5  ==  3
--    gradoNeg g2 1  ==  0
-- ---------------------------------------------------------------------

gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoNeg g v = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13. El grado de un v�rtice v de un grafo dirigido g, es el
-- n�mero de aristas de g que contiene a v. Si g es no dirigido, el
-- grado de un v�rtice v es el n�mero de aristas incidentes en v, teniendo
-- en cuenta que los lazos se cuentan dos veces. Definir la funci�n    
--    grado :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (grado g v) es el grado del v�rtice v en el grafo g. Por
-- ejemplo, 
--    grado g1  5  ==  4
--    grado g2  5  ==  3
--    grado g2  1  ==  3 
--    grado g3  2  ==  4
--    grado g3  1  ==  2
--    grado g3  3  ==  2
--    grado g5  1  ==  2
--    grado g10 3  ==  4
--    grado g11 3  ==  4
-- ---------------------------------------------------------------------

grado :: (Ix v,Num p) => Grafo v p -> v -> Int
grado g v = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Comprobar con QuickCheck que para cualquier grafo g, la
-- suma de los grados positivos de los v�rtices de g es igual que la
-- suma de los grados negativos de los v�rtices de g.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaGrados:: Grafo Int Int -> Bool
prop_sumaGrados g = undefined

-- La comprobaci�n es
--    ghci> quickCheck prop_sumaGrados
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 15. En la teor�a de grafos, se conoce como "Lema del
-- apret�n de manos" la siguiente propiedad: la suma de los grados de
-- los v�rtices de g es el doble del n�mero de aristas de g. 
-- Comprobar con QuickCheck que para cualquier grafo g, se verifica
-- dicha propiedad.
-- ---------------------------------------------------------------------

prop_apretonManos:: Grafo Int Int -> Bool
prop_apretonManos g = undefined

-- La comprobaci�n es
--    ghci> quickCheck prop_apretonManos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 16. Comprobar con QuickCheck que en todo grafo, el n�mero
-- de nodos de grado impar es par. 
-- ---------------------------------------------------------------------

prop_numNodosGradoImpar :: Grafo Int Int -> Bool
prop_numNodosGradoImpar g = undefined

-- La comprobaci�n es
--    ghci> quickCheck prop_numNodosGradoImpar
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la propiedad
--   prop_GradoCompleto :: Int -> Bool
-- tal que (prop_GradoCompleto n) se verifica si todos los v�rtices del
-- grafo completo K(n) tienen grado n-1. Usarla para comprobar que dicha
-- propiedad se verifica para los grafos completos de grados 1 hasta 30.
-- ---------------------------------------------------------------------

prop_GradoCompleto :: Int -> Bool
prop_GradoCompleto n = undefined

-- La comprobaci�n es                       
--    ghci> and [prop_GradoCompleto n | n <- [1..30]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 18. Un grafo es regular si todos sus v�rtices tienen el
-- mismo grado. Definir la funci�n
--    regular :: (Ix v,Num p) => Grafo v p -> Bool
-- tal que (regular g) se verifica si todos los nodos de g tienen el
-- mismo grado. 
--    regular g1            ==  False
--    regular g2            ==  False
--    regular (completo 4)  ==  True
-- ---------------------------------------------------------------------

regular :: (Ix v,Num p) => Grafo v p -> Bool
regular g = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la propiedad
--    prop_CompletoRegular :: Int -> Int -> Bool
-- tal que (prop_CompletoRegular m n) se verifica si todos los grafos
-- completos desde el de orden m hasta el de orden m son regulares y
-- usarla para comprobar que todos los grafos completo desde el de orden
-- 1 hasta el de orden 30 son regulares.
-- --------------------------------------------------------------------- 

prop_CompletoRegular :: Int -> Int -> Bool
prop_CompletoRegular m n = undefined

-- La comprobaci�n es
--    ghci> prop_CompletoRegular 1 30
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 20. Un grafo es k-regular si todos sus v�rtices son de
-- grado k. Definir la funci�n
--    regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
-- tal que (regularidad g) es la regularidad de g. Por ejemplo,
--    regularidad g1              ==  Nothing
--    regularidad (completo 4)    ==  Just 3
--    regularidad (completo 5)    ==  Just 4
--    regularidad (grafoCiclo 4)  ==  Just 2
--    regularidad (grafoCiclo 5)  ==  Just 2
-- ---------------------------------------------------------------------

regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
regularidad g = undefined    

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la propiedad
--    prop_completoRegular :: Int -> Bool
-- tal que (prop_completoRegular n) se verifica si el grafo completo de
-- orden n es (n-1)-regular. Por ejemplo,
--    prop_completoRegular 5  ==  True
-- y usarla para comprobar que la cumplen todos los grafos completos
-- desde orden 1 hasta 20.
-- ---------------------------------------------------------------------

prop_completoRegular :: Int -> Bool
prop_completoRegular n = undefined

-- La comprobaci�n es                       
--    ghci> and [prop_completoRegular n | n <- [1..20]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la propiedad
--    prop_cicloRegular :: Int -> Bool
-- tal que (prop_cicloRegular n) se verifica si el grafo ciclo de orden
-- n es 2-regular. Por ejemplo, 
--    prop_cicloRegular 2  ==  True
-- y usarla para comprobar que la cumplen todos los grafos ciclos
-- desde orden 3 hasta 20.
-- ---------------------------------------------------------------------

prop_cicloRegular :: Int -> Bool
prop_cicloRegular n = undefined

-- La comprobaci�n es                       
--    ghci> and [prop_cicloRegular n | n <- [3..20]]
--    True

-- ---------------------------------------------------------------------
-- � Generador de grafos                                              --
-- ---------------------------------------------------------------------

-- (generaGND n ps) es el grafo completo de orden n tal que los pesos
-- est�n determinados por ps. Por ejemplo,
--    ghci> generaGND 3 [4,2,5]
--    (ND,array (1,3) [(1,[(2,4),(3,2)]),
--                     (2,[(1,4),(3,5)]),
--                      3,[(1,2),(2,5)])])
--    ghci> generaGND 3 [4,-2,5]
--    (ND,array (1,3) [(1,[(2,4)]),(2,[(1,4),(3,5)]),(3,[(2,5)])])
generaGND :: Int -> [Int] -> Grafo Int Int
generaGND n ps  = creaGrafo ND (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n], x < y]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- (generaGD n ps) es el grafo completo de orden n tal que los pesos
-- est�n determinados por ps. Por ejemplo,
--    ghci> generaGD 3 [4,2,5]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[]),
--                    (3,[])])
--    ghci> generaGD 3 [4,2,5,3,7,9,8,6]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[(1,3),(2,7),(3,9)]),
--                    (3,[(1,8),(2,6)])])
generaGD :: Int -> [Int] -> Grafo Int Int
generaGD n ps = creaGrafo D (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n]]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- genGD es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGD
--    (D,array (1,4) [(1,[(1,1)]),(2,[(3,1)]),(3,[(2,1),(4,1)]),(4,[(4,1)])])
--    (D,array (1,2) [(1,[(1,6)]),(2,[])])
--    ...
genGD :: Gen (Grafo Int Int)
genGD = do n <- choose (1,10)
           xs <- vectorOf (n*n) arbitrary
           return (generaGD n xs)

-- genGND es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGND
--    (ND,array (1,1) [(1,[])])
--    (ND,array (1,3) [(1,[(2,3),(3,13)]),(2,[(1,3)]),(3,[(1,13)])])
--    ...
genGND :: Gen (Grafo Int Int)
genGND = do n <- choose (1,10)
            xs <- vectorOf (n*n) arbitrary
            return (generaGND n xs)

-- genG es un generador de grafos. Por ejemplo,
--    ghci> sample genG
--    (D,array (1,3) [(1,[(2,1)]),(2,[(1,1),(2,1)]),(3,[(3,1)])])
--    (ND,array (1,3) [(1,[(2,2)]),(2,[(1,2)]),(3,[])])
--    ...
genG :: Gen (Grafo Int Int)
genG = do d <- choose (True,False)
          n <- choose (1,10)
          xs <- vectorOf (n*n) arbitrary
          if d then return (generaGD n xs)
               else return (generaGND n xs)

-- Los grafos est� contenido en la clase de los objetos generables
-- aleatoriamente. 
instance Arbitrary (Grafo Int Int) where
    arbitrary = genG
