{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Examen_05_2022 where 
    import Prelude hiding ((++),(-),(+), EQ)
    data N where { O :: N ; S :: N -> N } deriving Show

    {- Problema 1. [30p.]
    (a) Defina, sin utilizar funciones auxiliares, la funcion (-) :: N -> N -> N, que calcula la resta
    natural de dos numeros. Esto es, m - n da 0 si m < n.
    Por ejemplo: S(S(S O)) - S O = S(S O)
    S O - S(S(S O)) = O -}

    (-) :: N -> N -> N
    (-) = \m -> \n -> case m of {
        O -> O;
        S k -> case n of {
            O -> m;
            S j -> (k - j);
        }
    }

    {- (b) Demuestre que (∀m,∀n::N) (m - n) ≤ m.
    Puede asumir, sin necesidad de demostrar, las siguientes propiedades de ≤:
    L1≤ (∀n::N) 0 ≤ n
    L2≤ (∀n::N) n ≤ S n
    L3≤ (∀n::N) n ≤ n
    L4≤ Transitividad de ≤.  

    (∀m,∀n::N) (m - n) ≤ m

    Demostracion por induccion en m :: N:
        Caso base: Sea m = O, O - n <= O
            O - n
            = por def. de (-)
            O
            <= por L1
            O
        
        Paso inductivo: 
            Hipotesis 1: Sea m = k, k - n <= k
            Tesis 1: Sea m = S k, S k - n <= S k

            Demostracion de la tesis 1, por induccion en n :: N:
                Caso base: Sea n = 0, S k - O <= S k
                    S k - O
                    = por def. de (-)
                    S k
                    <= por L3
                    S k

                Paso inductivo:
                    Hipotesis 2: Sea n = a, S k - a <= S k
                    Tesis 2: Sea n = S a, S k - S a <= S k

                    Demostracion de la tesis 2:
                        S k - S a
                        = por def. de (-)
                        k - a
                        <= por hipotesis 1
                        k
                        <= por L2
                        S k
            
        LQQD. 
    
    Problema 2. [40p.]
    Considere el siguiente tipo A a de arboles con uno o tres hijos en cada nodo interno e informacion
    de tipo a en las hojas: -}

    data A a where { 
        H :: a -> A a;
        U :: A a -> A a ;
        T :: A a -> A a -> A a -> A a 
    } deriving (Eq,Show)

    --(a) Escriba la expresion Haskell to correspondiente al arbol de la letra, y de su tipo:
    arb :: A Int
    arb = T (H 1) (U (T (H 2) (H 3) (H 4))) (H 5)

    {- (b) Defina la funcion tres::A a -> N, que recibe un arbol de tipo A a y calcula la cantidad de
    nodos ternarios T que tiene. Puede utilizar la suma de naturales (+) :: N -> N -> N definida
    como (+) = \m n -> case m of { O -> n ; S x -> S (x + n)} -}

    (+) :: N -> N -> N
    (+) = \m n -> case m of { O -> n ; S x -> S (x + n)}

    tres :: A a -> N 
    tres = \t -> case t of {
        H i -> O;
        U pri -> tres pri;
        T pri seg ter -> S(tres pri + tres seg + tres ter);
    }

    {- (c) Defina la funcion sinTres::A a -> A a, que recibe un arbol de tipo A a y reemplaza todos los
    nodos ternarios por uno unario, descartando los hijos derecho e izquierdo de cada nodo ternario
    y quedandose solo con el del medio -}

    sinTres::A a -> A a
    sinTres = \t -> case t of {
        H i -> H i;
        U pri -> U (sinTres pri);
        T pri seg ter -> U (sinTres seg);
    }
    
    {- (d) Demuestre que (∀t::A a) tres (sinTres t) = O

        Demostracion por induccion en t :: A a

            Caso base: Sea t = H i, con i :: a cualquiera, tres (sinTres H i) = O
                tres (sinTres H i)          ?= O
                = por def. de sinTres        | O
                tres (H i)                   | O
                = por def. de tres           | O
                O                            | O

             => Por reduccion a la misma expresion, el caso base se cumple.

            Paso inductivo:
                CASO U:
                    Hipotesis 1: Sea t = arb :: A a, tres (sinTres arb) = O
                    Tesis 1: Sea t = U arb, tres (sinTres U arb) = O

                    Demostracion de la tesis:
                        tres (sinTres U arb)        ?= O
                        = por def. de sinTres        | O
                        tres (U (sinTres arb))       | O
                        = por def. de tres           | O
                        tres (sinTres arb)           | O
                        = por Hipotesis 1            | O
                        O                            | O

                     Reduccion a la misma expresion.

                CASO T:
                Hipotesis 2: Sea t = pri :: A a, tres (sinTres pri) = O
                Hipotesis 3: Sea t = seg :: A a, tres (sinTres seg) = O
                Hipotesis 4: Sea t = ter :: A a, tres (sinTres ter) = O
                Tesis: Sea t = T pri seg ter, tres (sinTres T pri seg ter) = O

                    Demostracion de la tesis:
                        tres (sinTres T pri seg ter)         ?= O
                        = por def. de sinTres                 | O
                        tres (U sinTres seg)                  | O
                        = por def. de tres                    | O
                        tres (sinTres seg)                    | O
                        = por Hipotesis 3                     | O
                        O                                     | O

                     Reduccion a la misma expresion.

        LQQD.


    Problema 3. [30p]
    Considere la siguiente definicion:-}

    g :: [a] -> T a -> Bool
    g = \m -> \t -> case t of {
        P y z -> case m of { 
            [] -> y; 
            x:xs -> g xs z
        };
        Q q s -> g s q;
        R -> False
    }

    {- (a) Defina el tipo T a para que la funcion g compile:
    data T a where { P ::... ; Q ::... ; R ::... } -}

    data T a where {
        P :: Bool -> T a -> T a; --nodo unario con info de tipo Bool
        Q :: T a -> [a] -> T a; --nodo unario con info de tipo [a]
        R :: T a; --hoja sin info
    }

    {- (b) Defina la funcion listar :: T a -> [a], que devuelve una lista conteniendo todos los elementos de las listas 
    que haya en los nodos de un arbol. Puede utilizar la concatenacion de listas, definida como: -}
    (++) :: [a] -> [a] -> [a]
    (++) = \l1 l2 -> case l1 of { [] -> l2 ; x:xs -> x : (xs ++ l2) }

    listar :: T a -> [a]
    listar = \t -> case t of {
        R -> [];
        P b arb -> listar arb;
        Q arb l -> l ++ (listar arb);
    }

    {- (c) Defina la funcion andT :: T Bool -> Bool, que calcula la conjuncion (el &&) de todos los
    booleanos de los nodos de un arbol de tipo T Bool, incluyendo los que estan en las listas.
    Puede utilizar la funcion and :: [Bool] -> Bool que calcula la conjuncion de los elementos de
    una lista de booleanos, definida como:
    and = \l -> case l of { [] -> True ; b:bs -> b && and bs }. -}

    andT :: T Bool -> Bool
    andT = \t -> case t of {
        R -> True;
        P b arb -> b && (andT arb);
        Q arb l -> (andT arb) && (and l);
    }





    











