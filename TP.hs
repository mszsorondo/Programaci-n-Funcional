type Circulo a = [a]
 
sonCirculosIguales :: Circulo Integer -> Circulo Integer -> Bool
sonCirculosIguales circ1 circ2  = f circ1 circ2 (getIndex (head circ1) circ2 )

 
f :: Circulo Integer -> Circulo Integer -> Integer -> Bool 
f [x] ys j = x == getElement ys j
f (x:xs) ys j = j>=0 && x == getElement ys j && faux 
              where faux = f xs ys (mod (j+1) (fromIntegral (length ys)) )

--(Toma un valor entero y e indica su ubicación dentro del círculo, siendo la primera 0)...
getIndex :: Integer -> Circulo Integer -> Integer
getIndex x [] = -1 
getIndex x (y:ys) | x/=y = 1 + getIndex x ys 
                  | otherwise = 0  
--(Toma un índice 'j' y devuelve el valor entero en dicha ubicación)...
getElement :: Circulo Integer -> Integer -> Integer 
getElement circ1 0 = head circ1
getElement circ1 j = getElement (tail circ1) (j-1)

--(Inserta un valor entero dado dentro de una lista, ubicándolo en el índice 'i')... 
insert :: Integer -> Circulo Integer -> Integer -> Circulo Integer
insert x ys 0 = x:ys
insert x (y:ys) i = y:insert x ys (i-1)

--(Inserta un valor entero dado dentro de todas las ubicaciones posibles de un circulo)...
insertM :: Integer -> Circulo Integer -> Integer -> [Circulo Integer]
insertM x ys 0 = [insert x ys 0]
insertM x ys i = [insert x ys i] ++ insertM x ys (i-1) 
 
--(Inserta un valor entero dado dentro de todas las ubicaciones posibles de una lista de círculos)...
insertCirculos :: Integer -> [Circulo Integer] -> Integer -> [Circulo Integer]
insertCirculos x [] i = []
insertCirculos x (y:ys) i = insertM x y (i-1) ++ insertCirculos x ys i
  
permutar :: Integer -> [Circulo Integer]
permutar 2 = [[1,2],[2,1]] 
permutar n = insertCirculos n (permutar (n-1)) n
 
-- Computa el largo de un circulo
largo :: Circulo Integer -> Integer
largo [] = 0
largo (x:xs) =  1 + largo xs

-- (Dado un circulo y un índice evalua si los elementos adyacentes a partir del índice son primos (evalúa de adelante hacia atrás))
esCirPrimo :: Circulo Integer -> Integer -> Bool
esCirPrimo xs 0 = esPrimo (getElement xs 0 + getElement xs ((largo xs)-1) )
esCirPrimo xs j = esPrimo adyacente && esCirPrimo xs (j-1)
                where 
                     adyacente = getElement xs j + getElement xs (j-1) 
-- (Es la función anterior evaluada desde el último elemento del circulo)
esCirculoPrimo :: Circulo Integer -> Bool
esCirculoPrimo xs = esCirPrimo xs ((largo xs)-1)


minimoDivisor :: Integer -> Integer
minimoDivisor n = mind n 2
 
mind :: Integer -> Integer -> Integer
mind n d | d^2>n = n
         | mod n d == 0 = d 
         | otherwise = mind n (d+1)
 
esPrimo :: Integer -> Bool
esPrimo p = (minimoDivisor p) == p
 
estaRepetidoPrimero :: [Circulo Integer] -> Bool
estaRepetidoPrimero (x:y:[]) = sonCirculosIguales x y
estaRepetidoPrimero (x:y:xs) = sonCirculosIguales x y || estaRepetidoPrimero (x:xs)
 
quitarRepetido :: [Circulo Integer] -> [Circulo Integer]
quitarRepetido (x:[]) = [x]
quitarRepetido (x:xs) | estaRepetidoPrimero (x:xs) = quitarRepetido xs
                      | otherwise = x:quitarRepetido xs
 
listaCirculosPrimos :: Integer -> [Circulo Integer]
listaCirculosPrimos n = filter esCirculoPrimo (permutar n)

--(computa el largo de una lista de ciculos)
largo2 :: [Circulo Integer] -> Integer
largo2 [] = 0
largo2 (x:xs) =  1 + largo2 xs

contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = largo2 (listaCirculosPrimos n)


f2 :: Circulo Integer -> Circulo Integer -> Integer -> Bool 
f2 [x] ys j = x == getElement ys j
f2 (x:xs) ys j = j>=0 && x == getElement ys j && faux 
              where faux = f2 xs ys (mod (j-1) (fromIntegral (length ys)) )

sonCirculosEspejados :: Circulo Integer -> Circulo Integer -> Bool
sonCirculosEspejados circ1 circ2  = f2 circ1 circ2 (getIndex (head circ1) circ2 )

--profesora: esEspejado cuando es igual por rotacion al reverso

