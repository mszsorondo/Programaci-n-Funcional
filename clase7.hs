espar :: Integer -> Bool
espar x = mod x 2 == 0 
listar :: a -> a -> a -> [a]
listar x y z = x : y : z : [] 

pertenece :: Eq a => a -> [a] -> Bool
pertenece x l     | l == [] = False
                  | x == head l = True
                  | otherwise = pertenece x (tail l)

primerMultiploDe45345 :: [Integer] -> Integer
primerMultiploDe45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiploDe45345 (tail l)

sumatoria :: [Integer] -> Integer
sumatoria xs | xs == [] = 0
             | otherwise = head xs + sumatoria(tail xs)
--con pattern matching

sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Integer
longitud [] = 0
longitud (_: xs ) = 1 + longitud xs

productoria :: [Integer] -> Integer
productoria xs | xs == [] = 1
               | otherwise = head xs * productoria (tail xs)

productoria2 :: [Integer] -> Integer
productoria2 [] = 1
productoria2 (xs) = head xs * productoria2 (tail xs)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n xs | xs == [] = []
            | otherwise = (head xs + n : sumarN n (tail xs))

sumarN2 :: Integer -> [Integer] -> [Integer]
sumarN2 n [] = []
sumarN2 n xs = (head xs + n : sumarN2 n (tail xs))

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (n:xs) | xs == [] = []
                      | otherwise = (n+n:sumarElPrimero (xs))

sumarElPrimero2 :: [Integer] -> [Integer]
sumarElPrimero2 (x:xs) = sumarN2 x (x:xs)

ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo xs
--otra forma de escribir el renglon anterior:
--ultimo (x:y:xs) = ultimo (y:xs)


sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo xs = (head xs + ultimo xs:sumarElUltimo(tail xs))

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) |espar x = x:(pares xs)
             |otherwise = pares xs

