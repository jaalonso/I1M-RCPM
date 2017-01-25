-- OrdenacionPorInsercion.hs
-- Ordenación por inserción.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    inserta :: Ord a => a -> [a] -> [a]
-- tal que (inserta e xs) inserta el elemento e en la lista xs delante
-- del primer elemento de xs mayor o igual que e. Por ejemplo, 
--    inserta 5 [2,4,7,3,6,8,10] == [2,4,5,7,3,6,8,10]  
-- ---------------------------------------------------------------------

inserta :: Ord a => a -> [a] -> [a]
inserta e []                  = [e]
inserta e (x:xs) | e <= x     = e : (x:xs) 
                 | otherwise  = x : inserta e xs    

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    ordena_por_insercion :: Ord a => [a] -> [a]
-- tal que (ordena_por_insercion xs) es la lista xs ordenada mediante
-- inserción, Por ejemplo,
--    ordena_por_insercion [2,4,3,6,3] == [2,3,3,4,6]  
-- ---------------------------------------------------------------------

ordena_por_insercion :: Ord a => [a] -> [a]
ordena_por_insercion []     = []
ordena_por_insercion (x:xs) = 
    inserta x (ordena_por_insercion xs)   
