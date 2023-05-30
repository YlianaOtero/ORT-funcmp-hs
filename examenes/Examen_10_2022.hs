{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Examen_10_2022 where 
    import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)
    import Lab2
    
    --PROBLEMA 1:
    --elim elimina todos los elementos de la lista para los que p da True
    elimp :: (a -> Bool) -> [a] -> [a]
    elimp = \p -> \l -> case l of {
        [] -> [];
        x:xs -> case (p x) of {
            True -> elimp p xs;
            False -> x:(elimp p xs);
        }
    }

    --elimNp elimina el n-esimo elemento de la lista para el que p da True
    elimNp :: N ->  (a  -> Bool) -> [a] -> [a]
    elimNp = \n -> \p -> \l -> case n of {
        O -> case l of {
            [] -> [];
            x:xs -> case (p x) of {
                True -> xs;
                False -> x:(elimNp O p xs);
            }
        };
        S k -> case l of {
            [] -> [];
            z:zs -> z:(elimNp k p zs);
        }
    }

    --elimp pero mas compacta
    elimNp2 :: N ->  (a  -> Bool) -> [a] -> [a]
    elimNp2 =  \n -> \p -> \l -> case l of {
				[]   -> [];
				x:xs -> case p x of {
							False -> x:(elimNp n p xs);
							True  -> case n of {
								O   -> xs;
								S y -> x:(elimNp y p xs)
							}
				}
			}


    
    --cant x l devuelve la cantidad de veces que aparece x en l
    cant :: Eq a => a -> [a] -> N 
    cant = \x -> \l -> case l of {
        [] -> O;
        z:zs -> case (z == x) of {
            True -> S(cant x zs);
            False -> cant x zs;
        }
    }

    --Usando algunas de las funciones anteriores, definer cants, que devuelve 
    -- para cada elem de la lista, cuantas veces aparece
    cants :: Eq a =>  [a] -> [(a, N)]
    cants = \l -> case l of {
        [] -> [];
        x:xs -> (x, cant x l):(cants ((elimp (==x)) xs));
    }

    --DEMOSTRACIONES:
    --(Vp::a->Bool)(Vl::[a])
    --length (elimp p l) <= length l
        --dem: por induccion en l::[a]
            --sea p::(a->Bool) cualquiera
            --CB) l = []:
                --length (elimp p []) <= length []
                --dem:
                    --length (elimp p [])
                    -- = por def elimp
                    --length []
                    -- = por def length
                    --O
                    -- <=por lema O<=n
                    --length []
            --PI) Sean x::a, xs::[a] cualesquiera, l  = x:xs:
                --HI) length (elimp p xs) <= length xs
                --TI) length (elimp p (x:xs)) <= length (x:xs)
                --dem por casos en p x:
                    --Caso p x = True:
                        --length (elimp p (x:xs))
                        -- = por def elimp: 
                        --length (elimp p xs)
                        -- <= por HI)  
                        --length xs
                        -- <= por lema n <= S n
                        --S(length xs)
                        -- = por def length
                        --length (x:xs)

                        --llegamos a que length (elimp p (x:xs)) <= length xs,
                        --y a que length xs <= length (x:xs)
                        --entonces por transitiva, length (elimp p (x:xs)) <= length (x:xs)

                    --Caso p x = False:
                        --length (elimp p (x:xs))
                        -- = por def elimp: 
                        --length (x:(elimp p xs))
                        -- = por def de length:
                        --S(length (elimp p xs))
                        -- <= por HI) y lema m <= n => S m <= S n
                        --S (length xs)
                        -- = por def de length
                        --length (x:xs)

    --PROBLEMA 2
    --Considere la siguiente definicion:
    f :: X a -> [a] -> N -> Bool
    f = \t l m -> case t of {
        A x y z -> case y of {
            0 -> True; 
            S n -> f x [z] n
        };
        B -> True;
        C i j k -> case i of {
            True -> f k l m; 
            False -> f j [] 0 
        }
    }

    -- (a) Defina el tipo X a para que la funcion g compile:
    data X a where {
        A :: X a -> N -> a -> X a; --nodo unario con info de tipo N y a
        B :: X a; --hoja sin info
        C :: Bool -> X a -> X a -> X a; --Nodo binario con info de tipo Bool
    }

    --Arboles para testear:
    test1 :: X Int;
    test1 = A (C (True) (A (B) (S (S O)) (9)) (B)) (S O) (7);

    test2 :: X Int;
    test2 = B;

    {- (b) Defina la funci´on hojas :: X a -> N que cuenta la cantidad de hojas de un ´arbol de tipo X a.
    Puede utilizar la suma de naturales. -}
    hojas :: X a -> N
    hojas = \t -> case t of {
        A hijo nat elem -> hojas hijo;
        B -> S O;
        C bool izq der -> hojas izq + hojas der;
    }

    {- (c) Defina la funci´on espejo :: X a -> X a, que devuelve el ´arbol espejo de un ´arbol de tipo X a (o
    sea, otro con los mismos elementos, pero con todos los sub´arboles transpuestos). -}
    espejo :: X a -> X a
    espejo = \t -> case t of {
        A hijo nat elem -> A (espejo hijo) nat elem;
        B -> B;
        C bool izq der -> C bool (espejo der) (espejo izq);
    }

    {- (d) Demuestre que (∀t :: X a)(∀l :: [a])(∀m :: N) f t l m = True, donde f es la funcion definida
    en el punto (a). 
    Demostracion por induccion en t :: X a:

    Paso base: 
        Caso t = B (hoja):
            (∀l :: [a]) (∀m :: N) f B l m = True
                f B l m          ?= True
                = por def. de f:  | True
                True              | True

                => Por reduccion a la misma expresion, esto se cumple.

    Paso inductivo:
        Caso t = A hijo n elem, donde hijo :: X a, n :: N, elem :: a cualesquiera:
            Hipotesis 1: (∀l :: [a]) (∀m :: N) f hijo l m = True
            Tesis 1: (∀l :: [a]) (∀m :: N) f (A hijo n elem) l m = True

                Demostracion de la tesis, por induccion en n :: N:

                Paso base: 
                    Caso n = O:
                        (∀l :: [a]) (∀m :: N) f (A hijo O elem) l m = True
                        f (A hijo O elem) l m   ?= True
                        = por def. de f:         | True
                        True                     | True

                        => Por reduccion a la misma expresion, esto se cumple.
                
                Paso inductivo:
                    Caso n = k :: N:
                    Hipotesis 2: (∀l :: [a]) (∀m :: N) f (A hijo k elem) l m = True
                    Tesis 2: (∀l :: [a]) (∀m :: N) f (A hijo (S k) elem) l m = True

                        f (A hijo (S k) elem) l m   ?= True
                        = por def. de f              | True
                        f hijo [elem] k              | True
                        = por Hipotesis 1            | True
                        True                         | True

                        => Por reduccion a la misma expresion, esto se cumple.

        Caso t = C b izq der, donde b :: Bool, izq :: X a, der :: X a cualesquiera:
            Hipotesis 3: (∀l :: [a]) (∀m :: N) f izq l m = True
            Hipotesis 4: (∀l :: [a]) (∀m :: N) f der l m = True
            Tesis: (∀l :: [a]) (∀m :: N) f (C b izq der) l m = True

                Demostracion de la tesis, por casos en b :: Bool:

                    Caso b = True: (∀l :: [a]) (∀m :: N) f (C True izq der) l m = True
                        f (C True izq der) l m  ?= True
                        = por def. de f:         | True
                        f der l m                | True
                        = por Hipotesis 4        | True
                    
                    => Por reduccion a la misma expresion, esto se cumple.

                    Caso b = False: (∀l :: [a]) (∀m :: N) f (C False izq der) l m = True
                        f (C False izq der) l m  ?= True
                        = por def. de f:         | True
                        f izq [] O               | True
                        = por Hipotesis 3        | True
                        True                     | True

                    => Por reduccion a la misma expresion, esto se cumple.


        LQQD -}


    
