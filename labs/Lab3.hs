{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Lab3 where 
    import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)
    import Lab2

    --EJ. 1: LISTAS
    --1. null :: [a]-> Bool, que devuelve True si una lista esta vacıa.
    null :: [a]-> Bool
    null = \l -> case l of {
        [] -> True;
        x:xs -> False;
    }

    --2. length :: [a]-> N, que calcula la longitud (cantidad de elementos) de una lista.
    length :: [a]-> N
    length = \l -> case l of {
        [] -> O;
        x:xs -> S(length xs);
    }

    --3. duplicate :: [a] -> [a] que recibe una lista y devuelve otra, donde cada aparicion de un
    --elemento de la lista original es duplicada.
    duplicate :: [a] -> [a]
    duplicate = \l -> case l of {
        [] -> [];
        x:xs -> x:x:(duplicate xs);
    }

    --4. sum :: [N] -> N, que suma todos los elementos de una lista de numeros.
    sum :: [N] -> N
    sum = \l -> case l of {
        [] -> O;
        x:xs -> x + sum xs;
    }

    --5. prod :: [N] -> N, que multiplica todos los elementos de una lista de numeros.
    prod :: [N] -> N
    prod = \l -> case l of {
        [] -> 1;
        x:xs -> x * prod xs;
    }

    --6) map :: (a->b) ->[a]-> [b] que aplica una funcion a todos los elementos de una lista.
    map :: (a->b) ->[a]-> [b]
    map = \f -> \l -> case l of {
        [] -> [];
        x:xs -> (f x):(map f xs);
    }

    --7) zipWith :: (a->b->c)-> [a]-> [b]-> [c], que construye una lista cuyos elementos se calculan
    --a partir de aplicar la funci´on dada a los elementos de las listas de entrada que ocurren en
    --la misma posici´on en ambas listas. Si una lista es m´as larga que otra, no se consideran los
    --elementos sobrantes.
    zipWith :: (a->b->c)-> [a]-> [b]-> [c]
    zipWith = \f -> \l1 -> \l2 -> case l1 of {
        [] -> l2;
        x:xs -> case l2 of {
            [] -> [];
            z:zs -> (x + z):(zipWith f xs zs);
        }
    }

    --8) filter :: (a->Bool) ->[a]-> [a], que recibe un predicado y una lista y devuelve la lista con
    --los elementos para los cuales el predicado es verdadero.
    filter :: (a->Bool) ->[a]-> [a]
    filter = \p -> \l -> case l of {
        [] -> [];
        x:xs -> case (p x) {
            True -> x:(filter p xs);
            False -> (filter p xs);
        }
    }

    --9) and :: [Bool]-> Bool, que calcula la conjunci´on (&&) de una lista de booleanos.
    and :: [Bool]-> Bool
    and = \l -> case l of {
        [] -> True;
        x:xs -> x && (and xs);
    }

    --10) or :: [Bool]-> Bool, que calcula la disyunci´on (||) de una lista de booleanos.
    or :: [Bool]-> Bool
    or = \l -> case l of {
        [] -> False;
        x:xs -> x || (or xs);
    }

    --11) count :: (a->Bool) ->[a]-> N, que recibe un predicado y una lista y calcula la cantidad de
    --elementos de una lista para los cuales el predicado es verdadero.
    count :: (a->Bool) ->[a]-> N
    count = \p -> \l -> length (filter(p l));

    --18. ultimo::[a] -> (a -> Bool) -> N que retorna el indice del utimo elemento de una lista
    --para el cual se cumple un predicado dado, empezando desde 1. Si no hay ningun elemento que
    --cumpla el predicado, debe devolverse 0.
    ultimo :: [a] -> (a -> Bool) -> N
    ultimo = \l -> \p -> case l of {
        [] -> O;
        x:xs -> case p x of {
            True -> S(ultimo xs p);
            False -> case ultimo xs p of {
                O -> O;
                S k -> S(ultimo xs p)
            }
        }
    }

    --20. lensum :: [[a]] -> N que suma los largos de las listas de una lista de listas.
    --Por ejemplo: lensum [[0,1,2],[3],[4,5,6],[],[7,8]] = 9.
    lensum :: [[a]] -> N
    lensum = length . concat

    --EJ. 2: PARES ORDENADOS
    data (a,b) where { (_,_)::a -> b -> (a,b) }
    --1) fst :: (a,b) -> a, que devuelve el primer componente de un par.
    --Por ejemplo: fst(True,6) = True
    fst :: (a,b) -> a
    fst = \(a, b) -> a 
    
    --2) snd :: (a,b) -> b, que devuelve el segundo componente de un par.
    --Por ejemplo: snd(True,(6,[1,2])) = (6,[1,2]).
    snd :: (a,b) -> b
    snd = \(a, b) -> b

    --4) menMay :: Ord a => [a] -> a -> ([a],[a]), que recibe una lista y un valor del mismo tipo
    --que los elementos de la lista, y divide a la lista en dos: la primera conteniendo todos los
    --elementos  de la lista que son menores que el valor dado, y la segunda con todos los mayores o
    --iguales.
    --Por ejemplo: menMay [3,6,8,2,1,5,9] 6 = ([3,2,1,5],[6,8,9])
    menMay :: Ord a => [a] -> a -> ([a],[a])
    menMay = \l -> \k -> -> case l of {
        [] -> ([], []);
        x:xs -> case (x < k) of {
            True -> (x:fst(menMay xs k), snd(menMay xs k));
            False -> (fst(menMay xs k), x:snd(menMay xs k));
        }
    }

    --Version con una unica llamada recursiva, en vez de dos:
    menMay2 :: Ord a => [a] -> a -> ([a],[a])
    menMay2 = \l -> \k -> -> case l of {
        [] -> ([], []);
        x:xs -> case (x < k) of {
            True -> case (menMay2 xs k) of {
                (men, may) -> (x:men, may);
            };
            False -> case (menMay2 xs k) of {
                (men, may) -> (men, x:may);
            }
        }  
    }

    --EJ. 3: FUNCIONES PARCIALES
    pre :: Bool -> String -> t -> t
    pre = \b s p -> case b of { False -> error s ; True -> p }

    --1) head :: [a]-> a, que devuelve el primer elemento de una lista no vacia.
    head :: [a]-> a
    head = \l -> pre (not (null l)) "head: lista vacia" (case l of {x:xs -> x})

    --2) tail :: [a]-> [a], que devuelve el resultado de quitar el el primer elemento de una lista no vacia
    tail :: [a]-> [a]
    tail = \l -> pre(not (null l)) "tail: lista vacia" (case l of {x:xs -> xs});

    --3) last :: [a]-> a, que devuelve el ultimo elemento de una lista no vacia.
    last :: [a]-> a
    last = \l -> pre(not (null l)) "last: lista vacia" (case l of {
                                                            x:xs -> case xs of {
                                                                [] -> x;
                                                                z:zs -> ultimo zs;
                                                            }
                                                        })

    --4) init :: [a]-> [a], que devuelve el resultado de quitar el ´ultimo elemento de una lista no vac´ıa.
    init :: [a]-> [a]
    init = \l -> pre(not(null l)) "init: lista vacia" (case l of {
                                                            x:xs -> case xs of {
                                                                [] -> [];
                                                                z:zs -> x:init xs
                                                            }
                                                        })

    --5) (!!) :: [a]-> N-> a, que devuelve el n-esimo elemento de una lista, empezando desde 0.
    (!!) :: [a]-> N-> a
    (!!) = \l -> pre(not (null l)) "(!!): lista vacia" (case l of {
                                                            x:xs -> case n of {
                                                                O -> x;
                                                                S k -> S(xs !! k)
                                                            }
                                                        })

    --6) minList :: Ord a =>[a]-> a, que devuelve el m´ınimo elemento de una lista.
    minList :: Ord a =>[a]-> a
    minList = \l -> pre(not (null l)) "minList: lista vacia" (case l of {
                                                                x:xs -> case xs of {
                                                                    [] -> x
                                                                    zs -> minimo minList xs x
                                                                }   
                                                            })

    --EJ. 4: MAS FUNCIONES SOBRE LISTAS
    --1) prefijo :: Eq a => [a]-> [a]-> Bool, que recibe dos listas y determina si la primera es
    --prefijo de la segunda.
    --Por ejemplo: prefijo [1,2] [1,2,3,4,5] = True.
    prefijo :: Eq a => [a]-> [a]-> Bool
    prefijo = \l1 -> \l2 -> case l2 of {
        [] -> False;
        x:xs -> case l1 of {
            [] -> True;
            z:zs -> z == x && prefijo zs xs;
        }
    }

    --2) take :: N -> [a] -> [a], que recibe un natural n y una lista l, y devuelve la lista con los
    --primeros n elementos de l. Si la lista tiene menos de n elementos, la devuelve entera. No usar
    --funciones auxiliares.
    take :: N -> [a] -> [a]
    take = \k -> \l -> case (length l <= n) of {
        True -> l;
        False -> case l of {
            x:xs -> take (k - (S O)) xs
        }
    }

    --3) drop :: N -> [a] -> [a], que recibe un natural n y una lista l, y devuelve la lista resultante
    --de quitar los primeros n elementos de l. Si la lista tiene menos de n elementos,devuelve la lista
    --vacia. No usar funciones auxiliares.







