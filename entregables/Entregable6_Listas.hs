{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Entregable6_Listas where 
    import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)

    data N where { O :: N ; S :: N -> N } deriving Show
    uno = S O
    dos = S uno
    tres = S dos
    cuatro = S tres
    cinco = S cuatro

    --Ejercicio 1:

    --Parte (a):
    primerosQue :: (a -> Bool) -> [a] -> [a]
    primerosQue = \p -> \l -> case l of {
        [] -> [];
        x:xs -> case (p x) of {
            True -> x:(primerosQue p xs);
            False -> [];
        }
    }

    --Parte (b):
    quitarMientras :: (a->Bool) -> [a] -> [a]
    quitarMientras = \p -> \l -> case l of {
        [] -> [];
        x:xs -> case (p x) of {
            True -> quitarMientras p xs;
            False -> x:xs;
        }
    }

    --Parte (c):
    --(∀ p::a->Bool)(∀ l::[a]) primerosQue p l ++ quitarMientras p l = l
        --dem. por induccion en l::[a]:
            --sea p::(a->Bool) cualquiera:
            --CB) l = []: primerosQue p [] ++ quitarMientras p [] = []
                --dem:
                    --primerosQue p [] ++ quitarMientras p [] 
                    -- = por def. de primerosQue:
                    --[] ++ quitarMientras p []
                    -- = por def. de quitarMientras:
                    --[] ++ []
                    -- = por def. de (++):
                    --[]
                --se cumple el caso base porque redujimos una expresion a la otra.

            --PI) l = x:xs, con x::a, y xs::[a] cualesquiera
                --HI) primerosQue p xs ++ quitarMientras p xs = xs
                --TI) primerosQue p x:xs ++ quitarMientras p x:xs = x:xs
                --dem. por casos en p x:
                    --Caso p x = True:
                        --primerosQue p x:xs ++ quitarMientras p x:xs 
                        -- = por def. de primerosQue, caso p x = True:
                        --x:(primerosQue p xs) ++ quitarMientras p x:xs 
                        -- = por def. de quitarMientras, caso p x = True:
                        --x:(primerosQue p xs) ++ quitarMientras p xs
                        -- = por def. de (==):
                        --x:(primerosQue p xs ++ quitarMientras p xs)
                        -- = por HI):
                        --x:xs
                    --se cumple el caso p x = True, porque redujimos una expresion a la otra.
                   
                    --Caso p x = False:
                        --primerosQue p x:xs ++ quitarMientras p x:xs 
                        -- = por def. de primerosQue, caso p x = False:
                        --[] ++ quitarMientras p x:xs 
                        -- = por def. de quitarMientras, caso p x = False:
                        --[] ++ x:xs
                        -- = por def. de (==):
                        --x:xs
                    --se cumple el caso p x = False, porque redujimos una expresion a la otra.
                --se cumplen ambos casos, por lo que la tesis queda demostrada.
        

    --Ejercicio 2:

    --Parte (a):
    primeros :: N -> [a] -> [a]
    primeros = \n -> \l -> case l of {
        [] -> [];
        x:xs -> case n of {
            O -> [];
            S k -> x:(primeros k xs);
        }
    }
    
    --Parte (b):
    --(∀ n::N)(∀ l::[a]) length (primeros n l) ≤ n

    --Para este ejercicio, use dos lemas adicionales:
    --Lemax:l : (Vx :: a)(Vl :: [a]) x:l = [x] ++ l, demostrado al final del todo.
    --Lemalength++: (∀l1 :: [a])(∀l2 :: [a]) length (l1 ++ l2) = length l1 + length l2, demostrado en el repartido de matematica de listas ("Listas - Parte 2" en Aulas)
    --La definicion de (+) tambien se encuentra abajo del todo.

    --(∀ n::N)(∀ l::[a]) length (primeros n l) ≤ n
        --dem. por induccion en l::[a]:
        --CB1) l = [], sea n::N cualquiera: length (primeros n []) ≤ n    
            --dem:
                --length (primeros n [])
                -- = por def. de primeros:
                --length []
                -- = por def. de length:
                --O
                -- <= por L1:
                --n
            --se cumple el caso.
        
        --PI) l = x:xs, con x::a, y xs::[a] cualesquiera
            --HI1) length (primeros n xs) ≤ n
            --TI1) length (primeros n x:xs) ≤ n
                --dem: por induccion n::N
                    --CB1) n = O: length (primeros O x:xs) ≤ O
                        --length (primeros O x:xs)
                        -- = por def. de primeros:
                        --length []
                        -- = por def. de length:
                        --O
                        -- <= por L2:
                        --O
                    --se cumple el caso.
                
                    --PI) n = S k, con k::N cualquiera
                        --HI2) length (primeros k x:xs) ≤ k
                        --TI2) length (primeros S k x:xs) ≤ S k
                            --dem:
                                --length (primeros S k x:xs)
                                -- = por def. de primeros:
                                --length (x:(primeros k xs))
                                -- = por Lemax:l :
                                --length ([x] ++ primeros k xs)
                                -- = por Lemalength++:
                                -- length [x] + length (primeros k xs)
                                -- = por def. de length:
                                --S(length []) + length (primeros k xs)
                                -- = por def. de length:
                                --S O + length (primeros k xs)
                                -- = por def. de +:
                                -- S(O + length (primeros k xs))
                                -- = por def. de +:
                                --S(length (primeros k xs))
                                -- <= por HI2) y L3:
                                --S k
                            --se cumple la tesis 2.

                --se cumple la tesis 1.


--Lemax:l: (Vx :: a)(Vl :: [a]) x:l = [x] ++ l
    --dem. por induccion en l::[a], con x::a cualquiera:
    --CB) l = []: x:[] = [x] ++ []
        --dem:
            --[x] ++ []
            -- = por def. de ++:
            --x:([] ++ [])
            -- = por def. de ++:
            --x:[]
        --se cumple el caso base porque logramos reducir a la misma expresion.

    --PI) l = z:zs, con z::a, y zs::[a] cualesquiera
        --HI) x:zs = [x] ++ zs
        --TI) x:z:zs = [x] ++ z:zs
            --dem:
                --[x] ++ z:zs
                -- = por def. de ++:
                --x:([] ++ z:zs)
                -- = por def. de ++:
                --x:z:zs
            --logramos reducir a la misma expresion.



--Lemalength++: (∀l1 :: [a])(∀l2 :: [a]) length (l1 ++ l2) = length l1 + length l2
--Donde (+) :: N -> N -> N es la suma de naturales, definida como:
--(+) = \n -> \m -> case n of {O -> m ; S k -> S (k + m)}