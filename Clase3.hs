suma x y = x + y
resta x y = x - y

modulo :: Integer -> Integer
modulo x | x>=0 = x
         | otherwise = -x


unidades :: Integer -> Integer
unidades x  | (modulo x) > 10 = mod x 10
            | (modulo x) <= 10 = modulo x

sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
sumaUnidades3 x1 x2 x3 = unidades x1 + unidades x2 + unidades x3

todosImpares :: Integer -> Integer -> Integer -> Bool
todosImpares x1 x2 x3 = (espar x1) && (espar x2) && (espar x3)

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosUnImpar x1 x2 x3 = (not espar x1) || (not espar x2) || (not espar x3)

alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares x1 x2 x3 |((not espar x1) && (not espar x2)) = True
                           |((not espar x3) && (not espar x2)) = True
                           |((not espar x3) && (not espar x1)) = True
                           |otherwise = False

r1 :: Integer -> Integer -> Bool
r1 a b = espar (a+b)

r2 :: Integer -> Integer -> Bool
r2 a b = mod (2a+3b) 5 == 0

r3 :: Integer -> Integer -> Bool
r3 a b = unidades a == unidades b && unidades b == unidades (a*b)

r4 :: Num a => a -> a -> Bool
r4 x y = (x<3 && y<3) || (x>=3 && y>=3) 
--r5 :: Num a => a -> a -> Bool
--r5 x y = (x<3 && y<3) || (7>x>=3 && 7>y>=3) || (x>=7 && y>=7)

r6 :: (Integral a) => (a,a) -> (a,a) -> Bool
r6 x y k = x == k*y

espar :: Integer -> Bool
espar x = mod x 2 == 0 

r7 :: (Integral a, Floating k)=> (a,a) -> (a,a) -> Bool
r7 x y = x == k*y

