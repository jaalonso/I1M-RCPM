-- CambioDeBase.hs
-- Cambios de base: decimal y binaria.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  3 de Abril de 2011
-- ---------------------------------------------------------------------

-- Los números binarios se representan mediante listas de bits en orden
-- inverso. Un bit es cero o uno. Por ejemplo, el número 1101 se
-- representa por [1,0,1,1]. 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    bin2int :: [Int] -> Int
-- tal que (bin2int x) es el número decimal correspondiente al número
-- binario x. Por ejemplo, 
--    bin2int [1,0,1,1]  ==  13
-- ---------------------------------------------------------------------

bin2int :: [Int] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    int2bin :: Int -> [Int]
-- tal que (int2bin x) es el número binario correspondiente al número
-- decimal x. Por ejemplo, 
--    int2bin 13  ==  [1,0,1,1]  
-- ---------------------------------------------------------------------

int2bin :: Int -> [Int]
int2bin n | n < 2     = [n]
          | otherwise = n `mod` 2 : int2bin (n `div` 2)
