-- Ejercicios de ex�menes sobre grafos.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducci�n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se presenta una recopilaci�n de ejercicios sobre
-- grafos propuestos en ex�menes de la asignatura.
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
-- � Librer�as auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- Hay que elegir una librer�a 
import I1M.Grafo
-- import GrafoConVectorDeAdyacencia 
-- import GrafoConMatrizDeAdyacencia 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    recorridos :: [a] -> [[a]]
-- tal que (recorridos xs) es la lista de todos los posibles recorridos
-- por el grafo cuyo conjunto de v�rtices es xs y cada v�rtice se
-- encuentra conectado con todos los otros y los recorridos pasan por
-- todos los v�rtices una vez y terminan en el v�rtice inicial. Por
-- ejemplo, 
--    ghci> recorridos [2,5,3]
--    [[2,5,3,2],[5,2,3,5],[3,5,2,3],[5,3,2,5],[3,2,5,3],[2,3,5,2]]
-- Indicaci�n: No importa el orden de los recorridos en la lista.
-- ---------------------------------------------------------------------

recorridos :: [a] -> [[a]]
recorridos xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Consideremos un grafo G = (V,E), donde V es un
-- conjunto finito de nodos ordenados y E es un conjunto de arcos. En un
-- grafo, la anchura de un nodo es el m�ximo de los valores absolutos de
-- la diferencia entre el valor del nodo y los de sus adyacentes; y la
-- anchura del grafo es la m�xima anchura de sus nodos. Por ejemplo, en
-- el grafo  
--    grafo2 :: Grafo Int Int
--    grafo2 = creaGrafo ND (1,5) [(1,2,1),(1,3,1),(1,5,1),
--                                 (2,4,1),(2,5,1),
--                                 (3,4,1),(3,5,1),
--                                 (4,5,1)]
-- su anchura es 4 y el nodo de m�xima anchura es el 5.
-- 
-- Definir la funci�n 
--    anchura :: Grafo Int Int -> Int
-- tal que (anchuraG g) es la anchura del grafo g. Por ejemplo,
--    anchura g  ==  4
-- ---------------------------------------------------------------------

grafo2 :: Grafo Int Int
grafo2 = creaGrafo ND (1,5) [(1,2,1),(1,3,1),(1,5,1),
                             (2,4,1),(2,5,1),
                             (3,4,1),(3,5,1),
                             (4,5,1)]

anchura :: Grafo Int Int -> Int
anchura g = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar experimentalmente que la anchura del grafo
-- grafo c�clico de orden n es n-1.
-- ---------------------------------------------------------------------

-- La conjetura
conjetura :: Int -> Bool
conjetura n = undefined

-- La comprobaci�n es

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un grafo no dirigido G se dice conexo, si para cualquier
-- par de v�rtices u y v en G, existe al menos una trayectoria (una
-- sucesi�n de v�rtices adyacentes) de u a v.
--
-- Definirla funci�n 
--    conexo :: (Ix a, Num p) => Grafo a p -> Bool
-- tal que (conexo g) se verifica si el grafo g es conexo. Por ejemplo, 
--    conexo (creaGrafo ND (1,3) [(1,2,0),(3,2,0)])          ==  True
--    conexo (creaGrafo ND (1,4) [(1,2,0),(3,2,0),(4,1,0)])  ==  True
--    conexo (creaGrafo ND (1,4) [(1,2,0),(3,4,0)])          ==  False
-- ---------------------------------------------------------------------

conexo :: (Ix a, Num p) => Grafo a p -> Bool
conexo g = undefined

-- ---------------------------------------------------------------------
--  Ejercicio 4. Un mapa se puede representar mediante un grafo donde
--  los v�rtices son las regiones del mapa y hay una arista entre dos
--  v�rtices si las correspondientes regiones son vecinas. Por ejemplo,
--  el mapa siguiente 
--        +----------+----------+       
--        |    1     |     2    |       
--        +----+-----+-----+----+       
--        |    |           |    |       
--        | 3  |     4     | 5  |       
--        |    |           |    |       
--        +----+-----+-----+----+       
--        |    6     |     7    |       
--        +----------+----------+
-- se pueden representar por
--    mapa :: Grafo Int Int
--    mapa = creaGrafo ND (1,7)
--                     [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
--                      (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]
-- Para colorear el mapa se dispone de 4 colores definidos por   
--    data Color = A | B | C | D deriving (Eq, Show)
-- 
-- Definir la funci�n
--    correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
-- tal que (correcta ncs m) se verifica si ncs es una coloraci�n del
-- mapa m tal que todos las regiones vecinas tienen colores distintos. 
-- Por ejemplo, 
--    correcta [(1,A),(2,B),(3,B),(4,C),(5,A),(6,A),(7,B)] mapa == True
--    correcta [(1,A),(2,B),(3,A),(4,C),(5,A),(6,A),(7,B)] mapa == False
-- ---------------------------------------------------------------------

mapa :: Grafo Int Int
mapa = creaGrafo ND (1,7)
                 [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
                  (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]

data Color = A | B | C | E deriving (Eq, Show)

correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta ncs g = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Dado un grafo dirigido G, diremos que un nodo est�
-- aislado si o bien de dicho nodo no sale ninguna arista o bien no
-- llega al nodo ninguna arista. Por ejemplo, en el siguiente grafo
-- (Tema 22, pag. 31) 
--    grafo5 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
--                                (5,4,0),(6,2,0),(6,5,0)]
-- podemos ver que del nodo 1 salen 3 aristas pero no llega ninguna, por
-- lo que lo consideramos aislado. As� mismo, a los nodos 2 y 4 llegan
-- aristas pero no sale ninguna, por tanto tambi�n estar�n aislados.
--
-- Definir la funci�n 
--    aislados :: (Ix v, Num p) => Grafo v p -> [v]
-- tal que (aislados g) es la lista de nodos aislados del grafo g. Por
-- ejemplo, 
--    aislados grafo5 == [1,2,4]
-- ---------------------------------------------------------------------
                    
grafo5 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
                            (5,4,0),(6,2,0),(6,5,0)]
    
aislados :: (Ix v, Num p) => Grafo v p -> [v]
aislados g = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Consideremos una implementaci�n del TAD de los grafos,
-- por ejemplo en la que los grafos se representan mediante listas. Un
-- ejemplo de grafo es el siguiente:
--    grafo6 :: Grafo Int Int
--    grafo6 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
--                                (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]
-- 
-- Definir la funci�n 
--    conectados :: Grafo Int Int -> Int -> Int -> Bool
-- tal que (conectados g v1 v2) se verifica si los v�rtices v1 y v2
-- est�n conectados en el grafo g. Por ejemplo,
--    conectados grafo6 1 3  ==  True
--    conectados grafo6 1 4  ==  False
--    conectados grafo6 6 2  ==  False
--    conectados grafo6 2 6  ==  True
-- ----------------------------------------------------------------------------

grafo6 :: Grafo Int Int
grafo6 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
                            (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]

conectados :: Grafo Int Int -> Int -> Int -> Bool
conectados g v1 v2 = undefined

