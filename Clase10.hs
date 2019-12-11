module Clase10

where

type Polinomio = [Float]
type Monomio = (Float, Integer)


grado :: [Float] -> Integer
grado [] = -1
grado (x:xs) = 1 + grado xs

evaluar :: Polinomio -> Float -> Float
evaluar [] n = 0
evaluar (x:xs) n = x * (n ^ (grado (x:xs))) + evaluar xs n

derivada :: Polinomio -> Polinomio
derivada [] = []
derivada (x:xs) | grado (x:xs) == 0 = []
                | otherwise = [fromInteger (grado (x:xs)) * x] ++ derivada xs 

derivadaN n (x:xs) | n == 1 = derivada (x:xs)
                   | otherwise = derivada(derivadaN (n-1) (x:xs))

sumaAux :: Polinomio -> Polinomio -> Polinomio

sumaAux [] [] = []
sumaAux (f:fs) (g:gs) | grado (f:fs) == grado (g:gs) = [f + g] ++ suma fs gs
                      | grado (f:fs) > grado (g:gs) = [f] ++ suma fs (g:gs)
                      | grado (f:fs) < grado (g:gs) = [g] ++ suma (f:fs) gs

limpiar :: Polinomio -> Polinomio
limpiar [] = []
limpiar (x:xs) | x==0 = limpiar xs
               | otherwise = (x:xs) 

suma :: Polinomio -> Polinomio -> Polinomio
suma x y = limpiar(sumaAux x y)

productoEscalar :: Float -> Polinomio -> Polinomio
productoEscalar n [] = []
productoEscalar n (x:xs) = [n*x] ++ productoEscalar n xs

--ppm es productoPorMonomio
ppm :: (Float, Integer) -> Polinomio -> Polinomio
ppm (a,n) [] | n == 0 = []
             | otherwise = [fromInteger (0)] ++ ppm (a,n-1) []
ppm (a,n) (x:xs) = [a*x] ++ ppm (a,n) xs


--lo siguiente da mal, hay que corregirlo
productoAux :: Polinomio -> Polinomio -> Polinomio
productoAux [] (x:xs) = []
productoAux (y:ys) (x:xs) = (ppm (y, grado (y:ys)) (x:xs)) 

producto :: Polinomio -> Polinomio -> Polinomio
producto [] _ = []
producto (y:ys) (x:xs) = suma (productoAux (y:ys) (x:xs)) (productoAux ys (x:xs))

-- ¡Complejos! :)

type Complejos = (Float,Float)
