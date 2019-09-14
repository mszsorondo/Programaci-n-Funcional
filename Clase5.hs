module Clase5
()
where
import Clase4


eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | n>0 = 1 / fromInteger(factorial(n)) + eAprox(n-1)

e :: Float 
e = eAprox(100)

parteEntera :: Float -> Integer
parteEntera n | n<1 && n>(-1) = 0
             | n > 1 = 1 + parteEntera(n-1)
             | n < (-1) = -1 + parteEntera(n+1)

division :: Integer -> Integer -> (Integer , Integer) 
division a d | a > d = (1 + fst(division (a-d) d), snd(division(a-d) d))
             | a == d = (1,0)
             | a < d = (0,a-d*fst(division (a-d) d))
sumadivisiores :: Integer 
    