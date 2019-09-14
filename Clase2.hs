module Clase2
(esPar)
where 

-- haskell es un lenguaje fuertemente tipado (las expresiones tienen un tipo de datos y es muy estricto a la hora de evaluarlos)
-- LOS TIPOS SON CASE SENSITIVE
-- Integer!=Int pero por razones practicas no vamos a ver la diferencia

-- "div 10 3"division entera, pues es una operacion entre enteros

-- "mod 10 3"devuelve el resto del cociente

-- "10/4" devuelve 2.5, pues es una operacion entre numeros tipo  

-- ":t" evalúa el tipo de dato
--hay funciones que esperan enteros, otras que esperan racionales (float) y otras booleanos.

function3 :: Integer -> Integer -> Bool -> Bool
function3 x y b = b || (x > y)


doble :: Integer -> Integer
doble x = x + x

cuadruple :: Integer -> Integer
cuadruple x = x + x + x + x



esPar :: Integer -> Bool
esPar x = mod x 2 == 0 

espar2 :: Integer -> Bool
espar2 x    |mod x 2 == 0 = True
            |otherwise = False

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod x y == 0 

esMultiploDee :: Integer -> Integer -> Bool
esMultiploDee x y | mod x y == 0 = True
                  | otherwise = False

-- Variable de tipo: Entra cualquier tipo de dato y sale el mismo tipo de dato que entra. En la signatura se le dice 'a' por convencion
-- funcion id sirve para expresar un valor de tipo cualquiera identicamente a como se ingreso, pero como forma de funcion

-- Hay clases de tipos de datos, que especifican con mayor presicion el tipo de dato (como lo es Int o Float para NUM)
-- cuando se solicita el tipo de dato de una funcion a veces devuelve el tipo de
-- dato: hay relación de orden entre booleanos... True > False = True

triple :: Int -> Int
triple x = x + x + x

--En haskell TODO es función (sean varialbes, operaciones, expresiones etc), y la notacion original consiste en primero citar las...
-- ...funciones y luego los datos de entrada por lo tanto podriamos hacer una suma escribiendo primero la suma y luego los dos numeros...
-- ... que se suman
-- cuando decimos que f :: a -> a nos referimos a que entra un solo dato de tipo a y sale otro tipo de dato a (sea a el que sea)
-- cuando decimos que f :: a -> b nos referimos a que entra un tipo de dato y sale otro (sea cual sea a y b)

-- Tuplas: una tupla contiene elementos dentro y 'cuenta' como si fuese un solo dato de entrada.

tup :: (Integer, Bool) -> Bool
tup x = snd x || (fst x  < 10)

--LAS TUPLAS SON LA UNICA FORMA DE DEVOLVER DOS VALORES A PARTIR DE UNA funcion

crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)


invertir :: (a,b) -> (b,a)
invertir x = (snd x , fst x)

distanciaPuntos :: (Float, Float) -> Float
distanciaPuntos x = abs(fst x - snd x) 

f1 :: Float -> (Float, Float, Float)
f1 x = (2*x, x^2, x-7)

f2 :: Integer -> Integer
f2 n    | esPar 2 = div n 2
        | otherwise = n + 1

f3 :: Integer -> Integer
f3 n | mod n 6 == 0 = div (n^2) 2
     | otherwise = 3 * n + 1

g :: (Integer, Integer) -> Integer
g (n, m) = n * (m + 1)

