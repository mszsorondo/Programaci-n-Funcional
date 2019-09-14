f x y = x * x + y * y
g x y z = x + y + z * z

doble x = 2*x
suma x y = x + y
normaVectorial x1 x2 = (x1^2+x2^2)**(1/2)
funcionConstante8 x = 8
respuestaATodo = 42
signo n | n>0 =1 
    | n==0 =0 
    | n<0 =(-1)

absoluto n | n>0 = n
           | n<0 = (-n)
           | n == 0 = 0

maximo x y | x>y = x
           | x<y = y

maximo3 x y z | (x>y && x>z) = x
              | (y>x && y>z) = y
              | (z>x && z>y) = z

-- si se cumple doble condicion, sólo se lee la primera porque apenas se encuentra una condición cumplida las opciones restantes 
--se descartan

func n | n > 12 = 24 | n > 9 = 18 

--particularidad 'otherwise': siempre evalua a TRUE, asique si se encuentra un otherwise antes que cualquier otra condicion verdadera
--se va a devolver la sentencia que sigue a otherwise 

funcod n | otherwise = 5 | n>0 = 10