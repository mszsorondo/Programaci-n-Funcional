import Clase4


espar :: Integer -> Bool
espar x = mod x 2 == 0 


--pattern matching y mas recursion

negLogica :: Bool -> Bool
negLogica True = False;
negLogica False = True;

factorial2 :: Integer -> Integer
factorial2 0 = 1
factorial2 n = n * factorial(n-1)

--el pattern matching se hace dentro de cada variable
--funciones que no matchean..
--iguales :: Integer -> Integer
--iguales x x = True
--iguales x y = False;

--factorial :: Integer -> Integer
--factorial 0 = 1
--factorial (n+1) = (n+1) * factorial n

yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico True False = False
yLogico False True = False
yLogico False False = False


yLogico2 :: Bool -> Bool -> Bool
yLogico2 True True = True
yLogico2 _ _ = False



oLogico :: Bool -> Bool -> Bool
oLogico True True = True
oLogico True False = True
oLogico False True = True
oLogico False False = False

--usando el guion bajo represento a TODO el dominio, de modo que si meto cualquier cosa que no sea false false devolvera True
--si bien false false forma parte del dominio, la instruccion de oLogico2 para false false está escrita antes, de modo que si se 
--matechea el elemento del dominio, el resto de la funcion no se ejecuta
oLogico2 :: Bool -> Bool -> Bool
oLogico2 False False = False
oLogico2 _ _ = True



sumaGaussiana :: Integer -> Integer
sumaGaussiana (-1) = 0
sumaGaussiana n = n + sumaGaussiana(n-1)

algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0,_,_) = True
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero _ = False


productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1, x2) (y1, y2) = x1 * y1 + x2 * y2




-- por mi cuenta
--conjCollatz :: Integer -> (Integer, Integer)

--conjCollatz x   | x == 1 = (1,0)
--                | espar(x) = (div x 2, fst(conjCollatz(x-1)) )
--                | otherwise = (3 * (x-1) + 1, fst(conjCollatz(x-1)) )

parteEntera :: Float -> Integer
parteEntera n | n<1 && n>(-1) = 0
             | n > 1 = 1 + parteEntera(n-1)
             | n < (-1) = -1 + parteEntera(n+1)

digitosIguales :: Integer -> Bool
digitosIguales 1 = True
digitosIguales _ = 

--resueltos
sumdig :: Integer -> Integer
sumdig n | 0 <= n && n <= 9 
         |

pendiente: hacer ejercicios 1-5