module Clase4
(factorial)
where
import Clase2

fib :: Integer -> Integer
fib n | n==0 = 0
      | n==1 = 1
      | otherwise = fib(n-1) + fib(n-2)

factorial :: Integer -> Integer
factorial n | n==0 = 1
            | n > 0 = n * factorial(n-1)
            
sucA :: Integer -> Integer
sucA  n | n == 1 = 2
        | n > 1 = 2 * (n-1) * sucA(n-1) + 2^n * factorial(n-1) 

sucB :: Integer -> Integer
sucB n | n == 1 = 1
       | n == 2 = 2
       | n>2 = (n-2) * sucB(n-1) + 2 * (n-1) * sucB(n-2)

--cambio de variable...m=n+2
sucC :: Integer -> Integer
sucC m | m==1 = -3
       | m==2 = 6
       | ((m > 2) && not (esPar m)) = -sucC(m-1)-3
       | ((m > 2) && (esPar m)) = sucC(m-1) + 2*sucC(m-2) + 9

sumatoria :: Integer -> Integer
sumatoria n | n==1 = 1
            | n>1 = n + sumatoria(n-1)

f1 :: Integer -> Integer
f1 n | n==1 = 2
     | n>1 = 2^n + f1(n-1)

f2 :: Integer -> Float -> Float
f2 n q | n == 1 = q^1
       | n > 1 = q^n + f2(n-1) q

f3 :: Integer -> Float -> Float
f3 n q  | n == 0 = 0
        | n > 0 = q^(2*n) + q^(2*n-1) + (f3 (n-1) q)

--reutilizando codigo--

f3e :: Integer -> Float -> Float
f3e n q | n == 0 = 0
        | n < 2*n = f2 (2*n) q

--f4 :: Integer -> Float -> Float--
--f4 n q | n==0 =    | n > 0 = q ^ (2*n) + q ^ (2*n - 1) + f4 (n-1) q--




mul3 :: Integer -> Bool
mul3 n | n-3==0 = True
       | n-3<0 = False
       | n-3 /= 0 = mul3(n-3)

medioFact :: Integer -> Integer
medioFact n |n==1 = 1
            |n==2 = 2
            |n>2 = medioFact(n-2) * n

sumaImpares :: Integer -> Integer
sumaImpares n | n==1 = 1 
              | n>1 = nEsimoImpar  + sumaImpares(n-1)     
              where nEsimoImpar = 2*n - 1
                --where computa el resultado una sola vez y lo reutiliza
                --, lo cual acelera la ejecución

noTerminaNegativos :: Integer -> Integer
noTerminaNegativos m | m == 0 = 0
                     | otherwise = noTerminaNegativos(m-1)


--pendiente: ejercicio 19

