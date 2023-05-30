{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module EntrgableNaturales where
    import Prelude (Show)
    import Prelude (Integral)
    data N where { O :: N ; S :: N -> N } deriving Show
    data Bool where {False::Bool ; True::Bool} deriving Show

    --defino elementos naturales para hacer pruebas
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
    seis :: N 
    seis = S cinco
    siete :: N 
    siete = S seis
    ocho :: N 
    ocho = S siete

    (+) :: N -> N -> N
    (+) = \n -> \m -> case n of { --defino la suma para poder definir la multiplicacion
        O -> m;
        S k -> S (k + m);
    };    

    (*) :: N -> N -> N
    (*) = \n -> \m -> case n of {
                    O -> O;
                    S k -> m + (k * m);
                }


    doble :: N -> N  --esta funcion la uso para hacer pruebas
    doble = \n -> case n of { 
        O -> O;
        S x -> S(S(doble x));
    }

    not :: Bool -> Bool
    not = \b1 -> case b1 of {
        True -> False;
        False -> True;
    }

    par :: N -> Bool 
    par = \n -> case n of {
        O -> True; 
        S k -> not (par k)
    }

    positivo :: N -> Bool
    positivo = \n -> case n of {
        O -> False;
        S k -> True;
    };

    --EJERCICIO 1.
    mulfi :: N -> (N->N) -> N
    mulfi = \n -> \f -> case n of {
        O -> f O;
        S x -> (f (S x)) * (mulfi x f);
    }

    --EJERCICIO 2.
    mulfpi :: N -> (N -> N) -> (N -> Bool) -> N 
    mulfpi = \n -> \f -> \p -> case n of {
        O -> case (p O) of {
            True -> f O;
            False -> S (O);
        };
        S k -> case (p (S k)) of {
            True -> (f (S k)) * (mulfpi k f p);
            False -> (mulfpi k f p);
        }
    }

    --EJERCICIO 3.
    valle :: N -> N -> N -> Bool
    valle = \a -> \b -> \c -> case b of {
        O -> True;
        S x -> case a of {
            O -> False;
            S y -> case c of {
                O -> False;
                S z -> valle y x z;
            }
        }
    }

    --EJERCICIO 4.
    caida :: N -> N -> N 
    caida = \m -> \n -> case n of {
        O -> uno;
        S x -> case m of {
            O -> O;
            S y -> S y * (caida y x);
        }
    }