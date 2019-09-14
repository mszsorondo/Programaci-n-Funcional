module Clase5
where 
import Clase4

eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = 1/(fromInteger (factorial (n))) + eAprox(n - 1)

e :: Float
e = eAprox 100

parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = 1 + parteEntera(n-1)      

parteEnteraNeg :: Float -> Integer
parteEnteraNeg n | n >= 0 = parteEntera(n)
                 | n < 0 = (-1) + parteEnteraNeg(n+1)

division :: Integer -> Integer -> (Integer, Integer)
division x y | x < y = (0, x) 
             | x >= y = (1 + fst(divisionanterior), snd (divisionanterior))

             where divisionanterior = (division (x-y) y)

divisionZ :: Integer -> Integer -> (Integer, Integer)
divisionZ x y | x == 0 = (0, 0)
              | x < 0 = ((-1) + fst(divisionanterior2), snd (divisionanterior2))
              | otherwise = (division x y)
              where divisionanterior2 = (divisionZ (x+y) y)
              
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + (sumaDivisoresHasta n (k-1))
                       | otherwise = (sumaDivisoresHasta n (k-1))

sumaDivisores :: Integer -> Integer 
sumaDivisores n = sumaDivisoresHasta n n
