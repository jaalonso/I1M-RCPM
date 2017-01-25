-- Limite.hs
-- Límite de sucesiones.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    limite :: (Num a, Enum a, Num b, Ord b) => (a -> b) -> b -> b
-- tal que (limite f a) es el valor de f en el primer término x tal que 
-- para todo y entre x+1 y x+100, el valor absoluto de f(y)-f(x) es
-- menor que a. Por ejemplo,
--    limite (\n -> (2*n+1)/(n+5)) 0.001  ==  1.9900110987791344
--    limite (\n -> (1+1/n)**n) 0.001     ==  2.714072874546881
-- ---------------------------------------------------------------------

limite :: (Num a, Enum a, Num b, Ord b) => (a -> b) -> b -> b
limite f a = 
    head [f x | x <- [1..],
                maximum [abs(f y - f x) | y <- [x+1..x+100]] < a]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    esLimite :: (Num a, Enum a, Num b, Ord b) => (a -> b) -> b -> b -> Bool
-- tal que (esLimite f b a) se verifica si existe un x tal que para todo
-- y entre x+1 y x+100, el valor absoluto de f(y)-b es menor que a. Por
-- ejemplo, 
--    esLimite (\n -> (2*n+1)/(n+5)) 2 0.01     ==  True
--    esLimite (\n -> (1+1/n)**n) (exp 1) 0.01  ==  True
-- ---------------------------------------------------------------------

esLimite :: (Num a, Enum a, Num b, Ord b) => (a -> b) -> b -> b -> Bool
esLimite f b a =
    not (null [x | x <- [1..],
                   maximum [abs(f y - b) | y <- [x+1..x+100]] < a])
