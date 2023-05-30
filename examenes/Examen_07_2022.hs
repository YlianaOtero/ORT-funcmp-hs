{-#LANGUAGE GADTs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Examen_07_2022 where 
    import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split,(&&))
    --data N where { O :: N ; S :: N -> N } deriving Show
    import Lab2

    --PROBLEMA 1:
    {- (a) Defina la funcion fList :: N -> a -> (a->a) -> [a], tal que fList n x f retorna la lista
    [x,f x, f(f x),...f(f(..(f x)..))] (observar que tiene n+1 elementos).
    Ejemplos: fList (S(S(S 0))) True not = [True,False,True,False]
    fList (S(S 0)) (S 0) doble = [S 0, S(S 0), S(S(S(S 0)))] -}

    fList :: N -> a -> (a->a) -> [a]
    fList = \n -> \x -> \f -> case n of {
        O -> [x];
        S k -> x:(fList k (f x) f);
    }

    {- (b) Defina la funcion todos :: (a->Bool) -> [a] -> Bool, que recibe un predicado y una lista,
    y retorna True si y solo si el predicado se cumple para todos los elementos de la lista.
    Puede utilizar los conectivos booleanos que considere necesarios, defini´endolos como funciones
    auxiliares.
    Ejemplos: todos (> 4) [5,6,3] = False
    todos (== False) [False,False] = True-}

    todos :: (a->Bool) -> [a] -> Bool
    todos = \p -> \l -> case l of {
        [] -> True;
        x:xs -> (p x) && (todos p xs);
    }

    (&&) :: Bool -> Bool -> Bool
    (&&) = \a -> \b -> case a of {
        True -> b;
        False -> False;
    }

    {- (c) Demuestre que (∀n::N) todos not (fList n False id) = True, donde id::a -> a es la
    funcion identidad y not::Bool -> Bool es la negaci´on booleana definidas como:-}
    id::a -> a
    id = \x -> x

    {- not::Bool -> Bool
    not = \x -> case x of { 
        True -> False; 
        False -> True 
    }  -}


    {- (∀n::N) todos not (fList n False id) = True
    Demostracion por induccion en n :: N:

    Caso base: Sea n = O, todos not (fList O False id) = True
        todos not (fList O False id) ?= True
        = por def. de fList           | True
        todos not [False]             | True
        = por def. de todos           | True
        (not False) && (todos not []) | True
        = por def. de not             | True
        True && (todos not [])        | True
        = por def. de todos           | True
        True && True                  | True
        = por def. de &&              | True
        True                          | True

     => Se cumple por reduccion a la misma expresion.

    Paso inductivo:
        Hipotesis: Sea n = k, todos not (fList k False id) = True
        Tesis: Sea n = S k, todos not (fList (S k) False id) = True

        Demostracion de la tesis:
            todos not (fList (S k) False id)             ?= True
            = por def. de fList                           | True
            todos not (False:(fList k (id False) id))     | True
            = por def. de id                              | True
            todos not (False:(fList k False id))          | True
            = por def. de todos                           | True
            (not False) && (todos not (fList k False id)) | True
            = por def. de not                             | True
            True && (todos not (fList k False id))        | True
            = por hipotesis                               | True
            True && True                                  | True
            = por def. de &&                              | True
            True                                          | True

         => Se cumple por reduccion a la misma expresion.
        
    LQQD.-}

    
    --PROBLEMA 2:
    --Considere la siguiente funcion:
    f = \a b c -> case b of {
        O -> case c b of { 
            True -> not (c b);
            False -> c b
        };
        S x -> case a of { 
            [] -> f a x c;
            z:zs -> z && f zs x c
        }
    }
    --donde not es la negacion booleana definida en el ejercicio anterior, y && es la conjuncion definida como:
    {- (&&) :: Bool -> Bool -> Bool
    (&&) = \x y -> case x of { True -> y ; False -> False } -}

    --a) De el tipo de f.
    f :: [Bool] -> N -> (N -> Bool) -> Bool

    {- b) Demuestre que (∀a::..)(∀b::..)(∀c::..) f a b c = False.
    Puede asumir la conmutatividad de && sin necesidad de demostrarla. 
    
    (∀a :: [Bool])(∀b :: N)(∀c :: (N -> Bool)) f a b c = False

    Demostracion por induccion en b :: N:

    Caso base: Sea b = O, (∀a :: [Bool])(∀c :: (N -> Bool)) f a O c = False
        Voy a necesitar hacer casos en c O.
        Caso c O = True:
            f a O c             ?= False
            = por def. de f      | False
            not (True)           | False
            = por def. de not    | False
            False                | False

         => Se cumple por reduccion a la misma expresion.

        Caso c O = False:
            f a O c             ?= False
            = por def. de f      | False
            False                | False

         => Se cumple por reduccion a la misma expresion.


    Paso inductivo:
        Hipotesis 1: Sea b = k, (∀a :: [Bool])(∀c :: (N -> Bool)) f a k c = False
        Tesis 1: Sea b = S k, (∀a :: [Bool])(∀c :: (N -> Bool)) f a (S k) c = False

            Demostracion por induccion en a :: [Bool]:
            
            Caso base: Sea a = [], (∀c :: (N -> Bool)) f [] (S k) c = False
                f [] (S k) c        ?= False
                = por def. de f      | False
                f [] k c             | False
                = por Hipotesis 1    | False
                False                | False

             => Se cumple por reduccion a la misma expresion.

            Paso inductivo:
                Hipotesis 2: Sea a = xs, (∀c :: (N -> Bool)) f xs (S k) c = False
                Tesis 2: Sea a = x:xs, (∀c :: (N -> Bool)) f x:xs (S k) c = False

                Demostracion:
                    f x:xs (S k) c          ?= False
                    = por def. de f          | False
                    x && f xs k c            | False
                    = por Hipotesis 1        | False
                    x && False               | False
                    = por conmutatividad &&  | False
                    False && x               | False
                    = por def. de &&         | False
                    False                    | False

                 => Se cumple por reduccion a la misma expresion.

        LQQD.
    
    -}

    --PROBLEMA 3:
    {- Considere el siguiente tipo T a b de ´arboles ternarios con informaci´on de a en los nodos internos.
    y de tipo b las hojas: -}
    data T a b where { 
        H :: b -> T a b ;
        N :: a -> T a b -> T a b -> T a b -> T a b 
    }

    --(a) Escriba la expresi´on Haskell correspondiente al siguiente ´arbol y d´e su tipo.
    arb :: T Bool Int
    arb = N (True) (H 1) (N (False) (H 2) (N (False) (H 3) (H 4) (H 5)) (H 6)) (H 7)

    {- Defina la funcion altura :: T a b -> N, que recibe un arbol t y calcula su altura, esto es, la
    longitud de la rama mas larga del mismo. Puede utilizar la funcion max :: N -> N -> N, que
    calcula el maximo de dos numeros naturales.
    Para el ´arbol del ejemplo debera devolverse 3 (o sea, S(S(S 0))) -}

    altura :: T a b -> N
    altura = \t -> case t of {
        H b -> O;
        N a pri seg ter -> S(max (altura pri) (max (altura seg) (altura ter)));
    }

    {- (c) Defina la funcion medio :: T a b -> ([a],b), que dado un arbol t devuelva un par conteniendo:
    • una lista con los nodos de la rama del medio de t, y
    • la hoja del medio que se encuentra al final de esa rama.
    Puede utilizar las funciones fst :: (a,b) -> a y snd :: (a,b) -> b definidas como:-}
    fst :: (a,b) -> a
    fst = \p -> case p of {(x,y) -> x}

    snd :: (a,b) -> b
    snd = \p -> case p of {(x,y) -> y}
   -- Para el arbol del ejemplo debera devolverse el par ([True,False,False],4). 

    medio :: T a b -> ([a],b)
    medio = \t -> case t of {
        H b -> ([], b);
        N a pri sec ter -> (a:fst(medio sec), snd(medio sec));
    }

    {- (d) Demuestre que (∀t::T a b) length (fst (medio t)) ≤ altura t, donde length ::[a]->N
    es la funci´on que calcula la cantidad de elementos de una lista, definida como:
    length = \l -> case l of { [] -> 0 ; x:xs -> S(length xs) }.
    Puede asumir, sin necesidad de demostrar, las siguientes propiedades de ≤:
    L1: (∀n::N) 0 ≤ n
    L2: (∀n::N) n ≤ n
    L3: (∀m,n::N) m ≤ n ⇒ S m ≤ S n
    L4: (∀m,n::N) m ≤ max m n y n ≤ max m n
    L5: Transitividad de ≤

    (∀t::T a b) length (fst (medio t)) ≤ altura t
    Demostracion por induccion en t :: T a b

    Caso base: Sea t = H b, length (fst (medio H b)) ≤ altura H b
        length (fst (medio H b)) 
        = por def. de medio
        length (fst ([], b))
        = por def. de fst
        length ([])
        = por def. de length
        O
        ≤ por L1
        altura H b

    Paso inductivo: 
        Hipotesis 1: Sea t = pri, length (fst (medio pri)) ≤ altura pri
        Hipotesis 2: Sea t = seg, length (fst (medio seg)) ≤ altura seg
        Hipotesis 3: Sea t = ter, length (fst (medio ter)) ≤ altura ter
        Tesis: Sea t = N a pri seg ter, length (fst (medio (N a pri seg ter))) ≤ altura (N a pri seg ter)

        Demostracion de la tesis:
            length (fst (medio (N a pri seg ter)))
            = por def. de medio
            length (fst (a:fst(medio seg), snd(medio seg)))
            = por def. de fst
            length a:fst(medio seg)
            = por def. de length
            S(length fst (medio seg))
            <= por Hipotesis 2 y L3
            S(altura seg)
            <= por L3 y L4
            S(max (altura seg) (altura ter))
            <= por L3 y L4
            S(max (altura pri) (max (altura seg) (altura ter)))
            = por def. de altura
            altura (N a pri seg ter)
            ≤ por transitividad del <=
            altura (N a pri seg ter)

 -}






    


    