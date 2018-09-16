import Math.NumberTheory.Powers.Squares

dlist = [ x | x <- [2..10000], isSquare(x) == False]
isqr = integerSquareRoot

fnext:: (Integer,Integer,Integer)->(Integer,Integer,Integer)
fnext  (x,y,z)  = (x,a,b)
        where
         a = ((((isqr x) + y) `div` z)*z - y)
         b =  (x - ((((isqr x) + y) `div` z)*z - y)*((((isqr x) + y) `div` z)*z - y)) `div` z

fcycle :: (Integer,Integer,Integer)->Integer->Integer->Integer->Integer
fcycle (x,y,z) r1 r2 r3 = 
    if fnext(x,y,z) == (r1,r2,r3) then 1 else (fcycle(fnext(x,y,z)) r1 r2 r3) + 1

process_dlist:: [Integer]->[Integer]
process_dlist [] = []
process_dlist(d:ds) = fcycle (d,(isqr d),( d - (isqr d)*(isqr d)) ) (d) (isqr d) ( d - (isqr d)*(isqr d)): process_dlist(ds)

main = print $  length (filter odd (process_dlist(dlist)))
