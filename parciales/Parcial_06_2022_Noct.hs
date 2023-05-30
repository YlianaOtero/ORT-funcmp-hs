{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Parcial_06_2022_Noct where 
    import Prelude hiding ((++),(-),(+), (==), (<=), (*))
    data N where { O :: N ; S :: N -> N } deriving Show
{- 
    uno :: N
    uno = S O
    dos :: N
    dos = S uno
    tres :: N
    tres = S dos
    cuatro :: N 
    cuatro = S tres
    cinco :: N 
    cinco = S cuatro

    instance Eq N where 
    (==) = \n -> \m -> case n of {
                        O -> case m of {
                                O -> True;
                                S x -> False;
                        };
                        S y -> case m of {
                                O -> False;
                                S z -> y == z;
                        }
                    }


    instance Ord N where 
    (<=) = \n -> \m -> case n of {
                        O -> True;
                        S x -> case m of {
                                O -> False;
                                S y -> x <= y;
                        }
                    }


    instance Num N where
    (+) = \n -> \m -> case n of {
                        O -> m;
                        S k -> S (k + m);
                    }
    (-) = \n -> \m -> case n of {
                        O -> O;
                        S k -> case m of {
                            O -> n;
                            S l -> k - l;
                        };
                    }

    (*) = \n -> \m -> case n of {
                        O -> O;
                        S k -> m + (k * m);
                    }

    {- Ej. 1 [20p]
    a) Defina la funion creciente :: (N->N) -> N -> Bool, que recibe una funci´on f y un natural n,
    y retorna True si la funci´on es estrictamente creciente en el intervalo [0,n].
    Puede usar los operadores de orden de los naturales y los conectivos booleanos que considere
    necesarios.
    Ejemplo: creciente doble (S(S O)) = True. -}

    doble :: N -> N 
    doble = (S(S(O)) *)


    creciente :: (N->N) -> N -> Bool
    creciente = \f -> \n -> case n of {
        O -> True;
        S k -> ((f k ) <= (f (S k))) && creciente f k;
    }

    {- b) Defina la funcion splitIf :: (a->Bool) -> [a] -> ([a],[a]), tal que splitIf p l retorna
    un par de listas, la primera con los elementos de l para los cuales p da False, y la segunda con
    los elementos de l para los cuales p da True.
    Puede usar las funciones fst :: (a,b) -> a y snd :: (a,b) -> b, definidas como:
    fst = \p -> case p of {(x,y) -> x}
    snd = \p -> case p of {(x,y) -> y}
    Ejemplo: splitIf par [1,2,3,4,5] = ([1,3,5],[2,4]). -}

    par :: N -> Bool 
    par = \n -> case n of {
        O -> True; 
        S k -> not (par k)
    }

    splitIf :: (a->Bool) -> [a] -> ([a],[a])
    splitIf = \p -> \l -> case l of {
        [] -> ([], []);
        x:xs -> case (splitIf p xs) of {
            (falsos, verdaderos) -> case (p x) of {
                True -> (falsos, x:verdaderos);
                False -> (x:falsos, verdaderos)
            }
        }
    }  -}

    {- c) Defina la instancia de Eq para listas, o sea, la funci´on (==) :: Eq a => [a] -> [a] -> Bool.
    Puede usar los conectivos booleanos que considere necesarios. -}

    instance Eq a => Eq [a] where {
     --   (==) :: Eq a => [a] -> [a] -> Bool
        (==) = \l1 -> \l2 -> case l1 of {
            [] -> case l2 of {
                [] -> True;
                x:xs -> False;
            };
            x:xs -> case l2 of {
                [] -> False;
                z:zs -> z == x && xs == zs
            }
        }
    }
    

