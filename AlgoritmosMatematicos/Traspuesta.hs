-- Traspuesta.hs
-- Traspuesta de una matriz.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  3 de Abril de 2011
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    traspuesta :: [[a]] -> [[a]]
-- tal que (traspuesta m) es la traspuesta de la matriz m. Por ejemplo,
--    traspuesta [[1,2,3],[4,5,6]]    ==  [[1,4],[2,5],[3,6]]
--    traspuesta [[1,4],[2,5],[3,6]]  ==  [[1,2,3],[4,5,6]]
-- ---------------------------------------------------------------------

traspuesta :: [[a]] -> [[a]]
traspuesta []           = []
traspuesta ([]:xss)     = traspuesta xss
traspuesta ((x:xs):xss) = 
    (x:[h | (h:_) <- xss]) : traspuesta (xs : [t | (_:t) <- xss])
