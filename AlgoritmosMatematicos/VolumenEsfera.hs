-- VolumenEsfera.hs
-- Cálculo del volumen de una esfera. 
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 3 de Abril de 2011
-- ---------------------------------------------------------------------

module VolumenEsfera where

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función volumenEsfera tal que 
-- (volumenEsfera r) es el volumen de la esfera de radio r. Por ejemplo,
--    volumenEsfera 10  ==  4188.790204786391
-- Indicación: Usar la constante pi.
-- ---------------------------------------------------------------------

volumenEsfera r = (4/3)*pi*r^3

