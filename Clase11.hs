module Clase11
where
import Clase10




resta :: Polinomio -> Polinomio -> Polinomio
resta x y = suma x (productoEscalar (-1) y) 



primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente (x:xs) (y:ys) = (x/y, grado (x:xs) - grado (y:ys)) -- o sino primerCociente x y = (head x/head y, grado x - grado y)

primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto (x:xs) (y:ys) = resta (x:xs) (ppm (primerCociente (x:xs) (y:ys)) (y:ys))