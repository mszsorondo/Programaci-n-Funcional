import Clase5
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)


emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a, 1 , 0)  
emcd a b = (g,s,t)
            where (g, s', t') = emcd b (mod a b)
                  s = t'
                  t = s'- t'*q
                  q = div a b

tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m | mod b (mcd a m) == 0 = True
                    | otherwise = False

solucionPart a b m | tieneSolucion a b m = s * (div b g)
                    where (g,s,t) = (emcd a m)

solucionGen :: Integer -> Integer -> Integer -> (Integer, Integer)
solucionGen a b m | tieneSolucion a b m = (s * (div b g), div m g)
                    where (g,s,t) = (emcd a m)