{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Examen_10_2022_noct where 
    import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)
    import Lab2

    --mapAt n f l devuelve la lista resultadnte de aplicar f al n-esimo elemento de l (empezando desde 0),
    --y dejando el resto de los elems de la lista sin modificar.
    mapAt :: N -> (a -> a) -> [a] -> [a]
    mapAt = \n -> \f -> \l -> case l of {
        [] -> [];
        x:xs -> case n of {
            O -> (f x):xs;
            S k -> x:(mapAt k f xs);
        }
    }

    --DEMOSTRACIONES:
    --(Vn::N)(Vl::[a]) mapAt n id l = l;
        --dem. por induccion en l::[a]:
            --CB) l = [], sea n::N cualquiera: mapAt n id [] = []
                --dem:
                    --mapAt n id [] 
                    -- = por def. de mapAt:
                    -- [] (o sea l)
                -- se cumple el caso base porque redujimos una expresion a la otra.

            --PI) l = x:xs, con x::a, y xs::[a] cualesquiera
                --HI) (Vn:::N) mapAt n id xs = xs
                --TI) (Vn:::N) mapAt n id (x:xs) = x:xs
                    --dem: por induccion n::N
                        --CB) n = O: mapAt O id (x:xs) = x:xs
                                --mapAt O id (x:xs)
                                -- = por def. de mapAt:
                                --(id x):xs
                                -- = por def. de id:
                                --x:xs
                            -- se cumple el caso base porque redujimos una expresion a la otra.

                        --PI) n = S k, con k::N cualquiera
                            --HI2) mapAt k id (x:xs) = x:xs
                            --TI2) mapAt (S k) id (x:xs) = x:xs
                                --dem:
                                    --mapAt (S k) id (x:xs)
                                    -- = por def. de mapAt:
                                    --x:(mapAt k id xs)
                                    -- = por HI1 con n = k:
                                    --x:xs
                                -- se cumple el caso base porque redujimos una expresion a la otra.
                    --Vale porque lo demostramos en todos los casos.
        --Vale porque lo demostramos en todos los casos.

    --creciente recibe una funcion f y un natural n, y retorna True si la funcion es estrictamente creciente
    --en el intervalo [O, n]. Puede usar los operadores de orden de los naturales y los conectivos booleanes
    --que considere necesarios.
    creciente::(N->N) -> N -> Bool
    creciente = \f -> n -> case n of {
        O -> True;
        S k -> case (f (S k) > f k) of {
            True -> creciente f k;
            False -> False;
        }
    }

    --o lo que es lo mismo:
    creciente2::(N->N) -> N -> Bool
    creciente2 = \f -> n -> case n of {
        O -> True;
        S k -> f (S k) > f k && creciente f k;
    }

    --splitIf p l retorna un par de listas, la primera con los elems de l para los cuales p da false, y la
    --seg con los elems de l para los cuales p da true. 
    --Se pueden usar las siguientes funciones:
    fst :: (a,b) -> a
    fst = \(a, b) -> a 

    snd :: (a,b) -> b
    snd = \(a, b) -> b

    splitIf :: (a->Bool) -> [a] -> ([a], [a])
    splitIf = \p -> \l -> case l of {
        [] -> ([], [])
        x:xs -> case (p x) of {
            True -> (fst(splitIf p xs), x:snd(splitIf p xs));
            False -> (x:fst(splitIf p xs), snd(splitIf p xs));
        }
    }

    --forma alternativa:
    splitIf2 :: (a->Bool) -> [a] -> ([a], [a])
    splitIf2 = \p -> \l -> case l of {
        [] -> ([], [])
        x:xs -> case (splitIf2 p xs) of {
            (falsos, verdaderos) -> case p x of {
                True -> (falsos, x:verdaderos);
                False -> (x:falsos, verdaderos)
            }
        }
    }

    --IGUALDAD EN LISTAS
    (==) :: Eq a => [a] -> [a] -> Bool 
    (==) = \l1 -> \l2 -> case l1 of {
        [] -> case l2 of {
            [] -> True;
            _ -> False;
        }
        x:xs -> case l2 of {
            [] -> False;
            y:xs -> (x == y) && (xs == ys)
        }
    }

