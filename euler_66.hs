import Math.NumberTheory.Powers.Squares
import Data.List
import Data.Maybe

dlist =[ x | x <- [2..1000], isSquare(x) == False]

whileLoop:: Integer->Integer->Integer
whileLoop  d y =
         if isSquare'(1 + d*y*y) == True
                then  (1 + d*y*y)  else whileLoop  d (y+1)

process_dlist:: [Integer]->[Integer]
process_dlist [] = []
process_dlist(d:ds) = whileLoop d 1: process_dlist(ds)

list_xsqs = process_dlist(dlist)
main = print $  dlist !! fromJust (elemIndex (maximum list_xsqs) list_xsqs)

