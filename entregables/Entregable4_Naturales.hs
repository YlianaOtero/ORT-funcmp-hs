{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Entregable4_Naturales where
    import Prelude (Show)
    import Prelude (Integral)
    data N where { O :: N ; S :: N -> N } deriving Show
    data Bool where {False::Bool ; True::Bool} deriving Show

    --Defino algunos elementos naturales:
    uno :: N
    uno = S O
    dos :: N
    dos = S uno
    tres :: N
    tres = S dos

    --Funciones auxiliares o para testear:
    class Eq a where {
        (==) :: a -> a -> Bool;
        (/=) :: a -> a -> Bool
    }
    
    instance Eq N where {
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
    }

    instance Eq Bool where {
        (==) = \n -> \m -> case n of {
                            True -> m;
                            False -> not m;
                        }
    }

    (+) :: N -> N -> N
    (+) = \n -> \m -> case n of {
        O -> m;
        S k -> S (k + m);
    };    

    doble :: N -> N 
    doble = \n -> case n of { 
        O -> O;
        S x -> S(S(doble x));
    }

    positivo :: N -> Bool
    positivo = \n -> case n of {
        O -> False;
        S k -> True;
    };

    par :: N -> Bool 
    par = \n -> case n of {
        O -> True; 
        S k -> not (par k)
    }

    impar :: N -> Bool
    impar = \n -> not (par n)

    not :: Bool -> Bool
    not = \b1 -> case b1 of {
        True -> False;
        False -> True;
    }

    --EJERCICIOS:
    --1. 1):
    sinIg :: Eq a => N -> (N -> a) -> Bool
    sinIg = \n -> \f -> case n of {
                        O -> True;
                        S x -> case (f (S x) == f x) of {
                            True -> False;
                            False -> case x of {
                                O -> True;
                                S y -> not (f (S (S x)) == f (S x));
                            }
                        }
    }

    --1. 2) Demostracion (∀n::N) sinIg n par = True:
    --Defino el Lema parsk: (∀ k :: N) par (S k) == par k = False, y lo demostrare mas abajo.
    --Tambien usare como propiedad la simetria del == para naturales, la cual hemos probado en clase.

    --Primero vamos a demostrar Lema parsk:
        --(∀ k :: N) par (S k) == par k = False
        --dem. por induccion en k:
        --CB) k = O:
            --par (S O) == par O ?= False
            -- = por simetria del ==: par O == par (S O) ?= False
            -- = por def. de par, β y case: True == par (S O) ?= False
            -- = def. de par, β y case: True == False ?= False
            -- = def. de ==, βx2 y case: False = False
            -- Logramos reducir a la misma expresion.

        --PI) Para cualquier m :: N: par (S m) == par m = False

        --TI) par (S (S m)) == par (S m) = False
            --par (S (S m)) == par (S m) ?= False
            -- = tomando n = S m: par (S n) == par n ?= False
            -- la hipotesis la definimos para cualquier m :: N, particularmente sirve m = n, entonces:
            -- = por hipotesis: False = False
            -- Logramos reducir a la misma expresion.

        -- => Lema parsk: (∀ k :: N) par (S k) == par k = False
    

    --Ahora si demostramos la igualdad del ejercicio:
    --(∀n::N) sinIg n par = True
        --dem. por induccion en n:
        --CB) n = O:
            --sinIg O par ?= True
            -- = por def. sinIng, βx2 y case: True = True
            -- Logramos reducir a la misma expresion.

        --PI) Para cualquier k :: N: sinIg k par = True

        --TI) sinIg(S k) par = True
            -- sinIg(S k) par ?= True
            -- = por def. de sinIg, βx2 y case, y luego Lema parsk y casex2: not (par (S (S k)) == par (S k)) ?= True
            -- = por Lema parsk nuevamente: not (False) ?= True
            -- = por def. de not, β y case: True =  True
            -- Logramos reducir a la misma expresion.

        -- => (∀n::N) sinIg n par = True


    --2. 1):
    count :: (N -> Bool) -> N -> N
    count = \p -> \n -> case n of {
                        O -> case (p O) of {
                            True -> S O;
                            False -> O
                        };
                        S x -> case (p (S x)) of {
                            True -> S(count p x);
                            False -> count p x;
                        }
                            
    }

    --2. 2) Demostrar que (∀n::N) count par n + count impar n = S n
    --Voy a usar Lema +s: (∀m :: N)(∀n :: N) m + S n = S (m + n)
    --La demostracion de este lema se puede encontrar en el PDF de teorico "Naturales - Parte 3".
    --Tambien usare como propiedad la conmutatividad de la suma entre dos naturales, la cual hemos probado en clase.

    --(∀n::N) count par n + count impar n = S n
        --dem. por induccion en n :: N:
        --CB) count par O + count impar O ?= S O
            -- Trabajemos en reducir count par O:
                -- por def. de count, βx2 y case, y luego def. par, β y case, y luego case nuevamente: 
                -- count par O = S O

            -- Ahora trabajemos en reducir count impar O:
                -- por def. de count, βx2 y case, y luego def. impar y β, def. de not, β y case, y luego case nuevamente: 
                -- count impar O = O

            --Entonces obtenemos que:
            -- S O + O ?= S O
            -- = por def. de +, βx2 y case: S O = S O
            -- Logramos reducir a la misma expresion.

        --PI) Para cualquier k :: N: count par k + count impar k = S k

        --TI) count par (S k) + count impar (S k) = S (S k)
            -- count par (S k) + count impar (S k) ?= S (S k)
            -- dem. por casos en par (S k) :: Bool:
                -- Caso par (S k) = True:
                    -- = por def. de count, βx2 y casex2: S(count par k) + count impar (S k) ?= S (S k)
                    -- = por def. de count, βx2 y case, luego def. de impar y β, def. de not, β y case, y luego case nuevamente: S(count par k) + count impar k ?= S (S k)
                    -- = tomando m = count impar k, y n = count par k: S n + m ?= S (S k)
                    -- = por conmutatividad de la suma: m + S n ?= S (S k)
                    -- = por lema +s: S (m + n) ?= S (S k)
                    -- = deshaciendo el cambio de variable en m y en n y aplicando conmutatividad de la suma: S (count par k + count impar k) ?= S (S k)
                    -- = luego por hipotesis inductiva: S (S k) = S (S k)
                    -- Logramos reducir a la misma expresion.

                -- Caso par (S k) = False:
                    -- = por def. de count, βx2 y casex2: count par k + count impar (S k) ?= S (S k)
                    -- = por def. de count, βx2 y case, luego def. de impar y β, def. de not, β y case, y luego case nuevamente: count par k + S(count impar k) ?= S (S k)
                    -- = tomando m = count par k, y n = count impar k: m + S n ?= S (S k)
                    -- = por lema +s: S (m + n) ?= S (S k)
                    -- = deshaciendo el cambio de variable en m y en n: S (count par k + count impar k) ?= S (S k)
                    -- = luego por hipotesis inductiva: S (S k) = S (S k)
                    -- Logramos reducir a la misma expresion.

        -- => (∀n::N) count par n + count impar n = S n