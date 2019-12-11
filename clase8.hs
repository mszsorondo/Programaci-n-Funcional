--ejercicio 4, clase 5: ENTENDER Y completar POR MI CUENTA
--hacer suma interna y entender
--sumaExt n m q
  --  |n==1 = sumaInterna n m q
   -- |otherwise = sumaInterna n m q + sumaExt (n-1) m q


--
sumaInterior n 0 = 0 
sumaInterior n m = n * m + sumaInterior n (m-1)

sumaDoble 0 m = 0
sumaDoble n m = sumaInterior n m + sumaDoble (n-1) m

--

multDeN :: Integer -> [Integer] -> [Integer]
multDeN _ [] = []
multDeN n (x:xs)
    | mod x n == 0 = x : multDeN n xs
    | otherwise = multDeN n xs

--hacer ejercicios clase 7

-- termina el repaso --

type Set a = [a] -- para (entre nosotros, los del curso) decirnos que estamos con...
-- ... una lista en donde no importa el orden y no hay repetidos (como si fuese un cjto)
vacio :: Set Integer
vacio = []

-- agregar elementos a una lista



--
incluido :: Set Integer -> Set Integer -> Bool
incluido [] _ = True
incluido (x:xs) ys | elem x ys = incluido xs ys
                   | otherwise = False

iguales :: Set Integer -> Set Integer -> Bool
iguales xs ys = (incluido xs ys) && (incluido ys xs)

pertenece :: Set Integer -> Set [Integer] -> Bool
pertenece _ [] = False
pertenece cx (y:cy) = (iguales cx y) || pertenece cx cy


agregar cx cy | pertenece cx cy = cy
              | otherwise = (cx:cy)

agregarATodos :: Integer ->  Set (Set Integer) -> Set (Set Integer)
agregarATodos x [] = []
agregarATodos x (y:cy) | elem x y = y : agregarATodos x cy
                       | otherwise = ((x:y): agregarATodos x cy)